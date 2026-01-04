use std::path::{Path, PathBuf};

use anyhow::Result;
use crossterm::event::{self, Event, KeyCode};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode};
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Row, Table};

use crate::compare::{RunDiff, diff_runs};
use crate::database::{Database, RunInfo, SqliteDatabase};
use crate::format::format_duration;
use crate::model::RunResult;

/// Launch interactive TUI for comparing benchmark runs.
pub fn run_tui(db_path: &Path) -> Result<()> {
    let database = SqliteDatabase::open(db_path)?;
    let runs = database.list_runs(1000)?;
    drop(database);

    anyhow::ensure!(
        !runs.is_empty(),
        "No runs in database. Run benchmarks first with: cairo-metrics run"
    );
    anyhow::ensure!(runs.len() >= 2, "Need at least 2 runs to compare. Found only {}.", runs.len());

    enable_raw_mode()?;
    let mut terminal = ratatui::init();
    let mut app = App::new(runs, db_path.to_path_buf());

    loop {
        terminal.draw(|f| draw(&mut app, f))?;

        if let Event::Key(key) = event::read()?
            && app.handle_key(key.code)?
        {
            break;
        }
    }

    disable_raw_mode()?;
    ratatui::restore();
    Ok(())
}

fn load_run(db_path: &Path, id: &str) -> Result<RunResult> {
    let database = SqliteDatabase::open(db_path)?;
    database.load_run(id)
}

fn draw(app: &mut App, frame: &mut Frame<'_>) {
    match &app.screen {
        Screen::SelectBaseline => draw_select(app, frame, "Select BASELINE run"),
        Screen::SelectCurrent { baseline } => {
            draw_select(app, frame, &format!("Select CURRENT run (baseline: {})", baseline.id))
        }
        Screen::Compare { baseline, current, diff } => draw_compare(frame, baseline, current, diff),
    }
}

fn draw_select(app: &mut App, frame: &mut Frame<'_>, title: &str) {
    let items: Vec<ListItem<'_>> = app
        .runs
        .iter()
        .map(|r| {
            let commit = r.git_commit.as_deref().unwrap_or("-");
            let text = format!("{:<20} {} {}", r.id, &r.timestamp[..19], commit);
            ListItem::new(text)
        })
        .collect();

    let list = List::new(items)
        .block(Block::default().title(title).borders(Borders::ALL))
        .highlight_style(Style::default().bg(Color::DarkGray).bold())
        .highlight_symbol("> ");

    frame.render_stateful_widget(list, frame.area(), &mut app.list_state);

    let help = Paragraph::new("↑/↓: navigate | Enter: select | Backspace: back | q: quit")
        .style(Style::default().fg(Color::DarkGray));
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(0), Constraint::Length(1)])
        .split(frame.area());
    frame.render_widget(help, chunks[1]);
}

fn draw_compare(frame: &mut Frame<'_>, baseline: &RunInfo, current: &RunInfo, diff: &RunDiff) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(3), Constraint::Min(0), Constraint::Length(1)])
        .split(frame.area());

    let header_text = format!("Comparing: {} (baseline) vs {} (current)", baseline.id, current.id);
    let header = Paragraph::new(header_text).block(Block::default().borders(Borders::ALL));
    frame.render_widget(header, chunks[0]);

    let rows: Vec<Row<'_>> = diff
        .entries
        .iter()
        .map(|e| {
            let emoji = if e.delta_pct > 0.0 {
                "❌"
            } else if e.delta_pct < 0.0 {
                "✅"
            } else {
                "  "
            };
            let style = if e.delta_pct > 0.0 {
                Style::default().fg(Color::Red)
            } else if e.delta_pct < 0.0 {
                Style::default().fg(Color::Green)
            } else {
                Style::default()
            };
            Row::new(vec![
                e.name.clone(),
                format_duration(e.base_median),
                format_duration(e.current_median),
                format!("{:+.1}%", e.delta_pct),
                emoji.to_string(),
            ])
            .style(style)
        })
        .collect();

    let table = Table::new(
        rows,
        [
            Constraint::Percentage(40),
            Constraint::Percentage(15),
            Constraint::Percentage(15),
            Constraint::Percentage(15),
            Constraint::Percentage(15),
        ],
    )
    .header(
        Row::new(vec!["Benchmark", "Baseline", "Current", "Diff", ""])
            .style(Style::default().bold()),
    )
    .block(Block::default().title("Results").borders(Borders::ALL));

    frame.render_widget(table, chunks[1]);

    let help = Paragraph::new("Backspace: back to selection | q: quit")
        .style(Style::default().fg(Color::DarkGray));
    frame.render_widget(help, chunks[2]);
}

enum Screen {
    SelectBaseline,
    SelectCurrent { baseline: RunInfo },
    Compare { baseline: RunInfo, current: RunInfo, diff: RunDiff },
}

struct App {
    runs: Vec<RunInfo>,
    list_state: ListState,
    screen: Screen,
    db_path: PathBuf,
}

impl App {
    fn new(runs: Vec<RunInfo>, db_path: PathBuf) -> Self {
        let mut list_state = ListState::default();
        if !runs.is_empty() {
            list_state.select(Some(0));
        }
        Self { runs, list_state, screen: Screen::SelectBaseline, db_path }
    }

    fn selected_run(&self) -> Option<&RunInfo> {
        self.list_state.selected().and_then(|i| self.runs.get(i))
    }

    fn handle_key(&mut self, key: KeyCode) -> Result<bool> {
        match key {
            KeyCode::Char('q') | KeyCode::Esc => return Ok(true),
            KeyCode::Down | KeyCode::Char('j') => {
                if let Some(i) = self.list_state.selected()
                    && i < self.runs.len().saturating_sub(1)
                {
                    self.list_state.select(Some(i + 1));
                }
            }
            KeyCode::Up | KeyCode::Char('k') => {
                if let Some(i) = self.list_state.selected()
                    && i > 0
                {
                    self.list_state.select(Some(i - 1));
                }
            }
            KeyCode::Enter => {
                if let Some(run) = self.selected_run().cloned() {
                    match &self.screen {
                        Screen::SelectBaseline => {
                            self.screen = Screen::SelectCurrent { baseline: run };
                            self.list_state.select(Some(0));
                        }
                        Screen::SelectCurrent { baseline } => {
                            let baseline_result = load_run(&self.db_path, &baseline.id)?;
                            let current_result = load_run(&self.db_path, &run.id)?;
                            let diff = diff_runs(&baseline_result, &current_result);
                            self.screen =
                                Screen::Compare { baseline: baseline.clone(), current: run, diff };
                        }
                        Screen::Compare { .. } => {}
                    }
                }
            }
            KeyCode::Backspace => match &self.screen {
                Screen::SelectBaseline => {}
                Screen::SelectCurrent { .. } => {
                    self.screen = Screen::SelectBaseline;
                    self.list_state.select(Some(0));
                }
                Screen::Compare { baseline, .. } => {
                    self.screen = Screen::SelectCurrent { baseline: baseline.clone() };
                    self.list_state.select(Some(0));
                }
            },
            _ => {}
        }
        Ok(false)
    }
}
