use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use rusqlite::{Connection, params};

use crate::model::{BenchmarkResult, RunMeta, RunResult, TimingStats};

/// Database operations for storing and retrieving benchmark results.
pub trait Database {
    /// Store a run result (append-only, does not delete existing benchmarks).
    fn store_run(&self, id: &str, result: &RunResult) -> Result<()>;

    /// Load a run result by ID.
    fn load_run(&self, id: &str) -> Result<RunResult>;

    /// List all run IDs, ordered by timestamp (newest first).
    fn list_runs(&self, limit: u32) -> Result<Vec<RunInfo>>;

    /// Get history for a specific benchmark across runs.
    fn history(&self, benchmark: &str, limit: u32) -> Result<Vec<HistoryEntry>>;

    /// List all unique benchmark names.
    fn list_benchmarks(&self) -> Result<Vec<String>>;

    /// List benchmark names that have cached results for a specific run.
    fn list_benchmarks_for_run(&self, run_id: &str) -> Result<Vec<String>>;

    /// Delete all results for a run.
    fn purge_run(&self, run_id: &str) -> Result<()>;

    /// Delete only failed benchmarks for a run. Returns number deleted.
    fn purge_failed_benchmarks(&self, run_id: &str) -> Result<usize>;
}

/// SQLite implementation of the database.
pub struct SqliteDatabase {
    conn: Connection,
}

impl SqliteDatabase {
    /// Open or create a SQLite database at the given path.
    pub fn open(path: &Path) -> Result<Self> {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).with_context(|| {
                format!("failed to create database directory: {}", parent.display())
            })?;
        }

        let conn = Connection::open(path)
            .with_context(|| format!("failed to open database: {}", path.display()))?;

        let db = Self { conn };
        db.init_schema()?;
        Ok(db)
    }

    fn init_schema(&self) -> Result<()> {
        self.conn
            .execute_batch(
                "
            CREATE TABLE IF NOT EXISTS runs (
                id TEXT PRIMARY KEY,
                timestamp TEXT NOT NULL,
                git_commit TEXT
            );

            CREATE TABLE IF NOT EXISTS benchmark_results (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id TEXT NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
                benchmark TEXT NOT NULL,
                scenario TEXT NOT NULL,
                patch TEXT NOT NULL DEFAULT '',
                phase TEXT NOT NULL,
                metric TEXT NOT NULL,
                mean_ns INTEGER NOT NULL,
                stddev_ns INTEGER NOT NULL,
                median_ns INTEGER NOT NULL,
                min_ns INTEGER NOT NULL,
                max_ns INTEGER NOT NULL,
                mad_ns INTEGER NOT NULL,
                runs INTEGER NOT NULL,
                warmup INTEGER NOT NULL,
                success INTEGER NOT NULL,
                times_json TEXT,
                UNIQUE (run_id, benchmark, scenario, patch, phase, metric)
            );

            CREATE INDEX IF NOT EXISTS idx_runs_timestamp ON runs(timestamp);
            CREATE INDEX IF NOT EXISTS idx_benchmark_results_benchmark ON benchmark_results(benchmark);
            CREATE INDEX IF NOT EXISTS idx_benchmark_results_phase ON benchmark_results(phase);
            ",
            )
            .context("failed to initialize database schema")?;
        Ok(())
    }

    /// Create an in-memory database (for testing).
    #[cfg(test)]
    pub fn in_memory() -> Result<Self> {
        let conn = Connection::open_in_memory()?;
        let db = Self { conn };
        db.init_schema()?;
        Ok(db)
    }
}

impl Database for SqliteDatabase {
    fn store_run(&self, id: &str, result: &RunResult) -> Result<()> {
        let tx = self.conn.unchecked_transaction()?;

        tx.execute(
            "INSERT OR IGNORE INTO runs (id, timestamp, git_commit) VALUES (?1, ?2, ?3)",
            params![id, result.meta.timestamp_utc, result.meta.git_commit],
        )
        .with_context(|| format!("failed to store run '{}'", id))?;

        for bench in &result.benchmarks {
            let times_json = serde_json::to_string(&bench.timing.times_ns)
                .context("failed to serialize times")?;
            tx.execute(
                "INSERT OR REPLACE INTO benchmark_results
                 (run_id, benchmark, scenario, patch, phase, metric,
                  mean_ns, stddev_ns, median_ns, min_ns, max_ns, mad_ns,
                  runs, warmup, success, times_json)
                 VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14, ?15, ?16)",
                params![
                    id,
                    bench.benchmark,
                    bench.scenario,
                    bench.patch,
                    bench.phase,
                    bench.metric,
                    bench.timing.mean_ns,
                    bench.timing.stddev_ns,
                    bench.timing.median_ns,
                    bench.timing.min_ns,
                    bench.timing.max_ns,
                    bench.timing.mad_ns,
                    bench.timing.runs,
                    bench.timing.warmup,
                    bench.success,
                    times_json,
                ],
            )?;
        }

        tx.commit()?;
        Ok(())
    }

    fn load_run(&self, id: &str) -> Result<RunResult> {
        let (timestamp, git_commit): (String, Option<String>) = self
            .conn
            .query_row("SELECT timestamp, git_commit FROM runs WHERE id = ?1", params![id], |row| {
                Ok((row.get(0)?, row.get(1)?))
            })
            .with_context(|| format!("failed to load run '{}': not found", id))?;

        let mut stmt = self.conn.prepare(
            "SELECT benchmark, scenario, patch, phase, metric,
                    mean_ns, stddev_ns, median_ns, min_ns, max_ns,
                    mad_ns, runs, warmup, success, times_json
             FROM benchmark_results WHERE run_id = ?1",
        )?;

        let benchmarks: Vec<BenchmarkResult> = stmt
            .query_map(params![id], |row| {
                let times_json: Option<String> = row.get(14)?;
                let times_ns: Vec<u64> =
                    times_json.and_then(|s| serde_json::from_str(&s).ok()).unwrap_or_default();

                Ok(BenchmarkResult {
                    benchmark: row.get(0)?,
                    scenario: row.get(1)?,
                    patch: row.get(2)?,
                    phase: row.get(3)?,
                    metric: row.get(4)?,
                    timing: TimingStats {
                        times_ns,
                        mean_ns: row.get(5)?,
                        stddev_ns: row.get(6)?,
                        median_ns: row.get(7)?,
                        min_ns: row.get(8)?,
                        max_ns: row.get(9)?,
                        mad_ns: row.get(10)?,
                        runs: row.get(11)?,
                        warmup: row.get(12)?,
                    },
                    success: row.get(13)?,
                })
            })?
            .collect::<Result<Vec<_>, _>>()?;

        Ok(RunResult { meta: RunMeta { timestamp_utc: timestamp, git_commit }, benchmarks })
    }

    fn list_runs(&self, limit: u32) -> Result<Vec<RunInfo>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, timestamp, git_commit FROM runs ORDER BY timestamp DESC LIMIT ?1",
        )?;

        let rows = stmt.query_map(params![limit], |row| {
            Ok(RunInfo { id: row.get(0)?, timestamp: row.get(1)?, git_commit: row.get(2)? })
        })?;

        Ok(rows.collect::<Result<Vec<_>, _>>()?)
    }

    fn history(&self, display_name: &str, limit: u32) -> Result<Vec<HistoryEntry>> {
        // Build display name from columns using SQLite CASE expression.
        let mut stmt = self.conn.prepare(
            "SELECT r.id, r.timestamp, r.git_commit, b.median_ns
             FROM runs r
             JOIN benchmark_results b ON r.id = b.run_id
             WHERE CASE
                 WHEN b.patch != '' THEN b.benchmark || '-patched-' || b.patch || '-' || b.phase || '-' || b.metric
                 ELSE b.benchmark || '-' || b.scenario || '-' || b.phase || '-' || b.metric
             END = ?1
             ORDER BY r.timestamp DESC
             LIMIT ?2",
        )?;

        let rows = stmt.query_map(params![display_name, limit], |row| {
            Ok(HistoryEntry {
                run_id: row.get(0)?,
                timestamp: row.get(1)?,
                git_commit: row.get(2)?,
                median_ns: row.get(3)?,
            })
        })?;

        Ok(rows.collect::<Result<Vec<_>, _>>()?)
    }

    fn list_benchmarks(&self) -> Result<Vec<String>> {
        let mut stmt = self.conn.prepare(
            "SELECT DISTINCT benchmark, scenario, patch, phase, metric
             FROM benchmark_results
             ORDER BY benchmark, scenario, phase, metric",
        )?;
        let rows = stmt.query_map([], |row| {
            let benchmark: String = row.get(0)?;
            let scenario: String = row.get(1)?;
            let patch: String = row.get(2)?;
            let phase: String = row.get(3)?;
            let metric: String = row.get(4)?;
            let key = if patch.is_empty() {
                format!("{}-{}-{}-{}", benchmark, scenario, phase, metric)
            } else {
                format!("{}-patched-{}-{}-{}", benchmark, patch, phase, metric)
            };
            Ok(key)
        })?;
        Ok(rows.collect::<Result<Vec<_>, _>>()?)
    }

    fn list_benchmarks_for_run(&self, run_id: &str) -> Result<Vec<String>> {
        let mut stmt = self.conn.prepare(
            "SELECT benchmark, scenario, patch, phase, metric
             FROM benchmark_results WHERE run_id = ?1
             ORDER BY benchmark, scenario, phase, metric",
        )?;
        let rows = stmt.query_map(params![run_id], |row| {
            let benchmark: String = row.get(0)?;
            let scenario: String = row.get(1)?;
            let patch: String = row.get(2)?;
            let phase: String = row.get(3)?;
            let metric: String = row.get(4)?;
            let key = if patch.is_empty() {
                format!("{}-{}-{}-{}", benchmark, scenario, phase, metric)
            } else {
                format!("{}-patched-{}-{}-{}", benchmark, patch, phase, metric)
            };
            Ok(key)
        })?;
        Ok(rows.collect::<Result<Vec<_>, _>>()?)
    }

    fn purge_run(&self, run_id: &str) -> Result<()> {
        self.conn.execute("DELETE FROM benchmark_results WHERE run_id = ?1", params![run_id])?;
        self.conn.execute("DELETE FROM runs WHERE id = ?1", params![run_id])?;
        Ok(())
    }

    fn purge_failed_benchmarks(&self, run_id: &str) -> Result<usize> {
        let deleted = self.conn.execute(
            "DELETE FROM benchmark_results WHERE run_id = ?1 AND success = 0",
            params![run_id],
        )?;
        Ok(deleted)
    }
}

/// Information about a stored run.
#[derive(Debug, Clone)]
pub struct RunInfo {
    pub id: String,
    pub timestamp: String,
    pub git_commit: Option<String>,
}

/// A single entry in the history of a benchmark.
#[derive(Debug)]
pub struct HistoryEntry {
    pub run_id: String,
    pub timestamp: String,
    pub git_commit: Option<String>,
    pub median_ns: u64,
}

#[cfg(test)]
#[path = "database_test.rs"]
mod tests;
