#!/usr/bin/env python3
"""
Script to test different inline-strategy values and measure their impact on
contract size and L2 gas consumption.

The script tests each specified inline-strategy value, measuring contract size
and performance (L2 Gas). Results are compared against a test overhead
of TEST_OVERHEAD gas to calculate per-settlement efficiency.

Optional 'avoid' parameter: Include 'avoid' in your list to establish a baseline
reference. This sets inline-strategy to the string "avoid" in the config files,
which uses Cairo's default inline strategy behavior.

Checkpoints are saved after each test to inline_strategy_checkpoint.json,
allowing you to track progress and resume if interrupted.

Usage:
    python ./scripts/test_inline_strategy.py [strategy_values...] [--plot-only]

Examples:
    python ./scripts/test_inline_strategy.py 5 10 15 20 25 30
    python ./scripts/test_inline_strategy.py avoid 10 20 30 40 50
    python ./scripts/test_inline_strategy.py --plot-only
    python ./scripts/test_inline_strategy.py  # Default: 'avoid' + range(0, 101, 5)
"""

import subprocess
import json
import re
import sys
import os
import argparse
from pathlib import Path
import matplotlib.pyplot as plt
from typing import List, Tuple
from datetime import datetime
from script_utils import get_project_root

TEST_OVERHEAD = 1411200
AVOID_STRATEGY = "avoid"


class InlineStrategyTester:
    def __init__(self, repo_path: str):
        self.repo_path = Path(repo_path)
        self.scarb_toml_path = self.repo_path / "Scarb.toml"
        self.perpetuals_scarb_toml_path = (
            self.repo_path / "workspace/apps/perpetuals/contracts/Scarb.toml"
        )
        self.bytecode_path = (
            self.repo_path / "target/dev/perpetuals_Core.compiled_contract_class.json"
        )
        self.checkpoint_file = self.repo_path / "inline_strategy_checkpoint.json"

    def update_inline_strategy(self, value) -> None:
        """Update the inline-strategy value in both Scarb.toml files."""
        print(f"Setting inline-strategy to {value}")

        # For 'avoid', write it as a string; for numbers, write as integers
        if value == AVOID_STRATEGY:
            replacement_value = f'"{AVOID_STRATEGY}"'
        else:
            replacement_value = str(value)
        pattern = (
            rf"(\[profile\.dev\.cairo\].*?inlining-strategy\s*=\s*)" rf'(?:\d+|"{AVOID_STRATEGY}")'
        )

        # Update root Scarb.toml
        with open(self.scarb_toml_path, "r") as f:
            content = f.read()

        # Update dev profile
        content = re.sub(pattern, rf"\g<1>{replacement_value}", content, flags=re.DOTALL)

        # Update release profile
        pattern_release = (
            rf"(\[profile\.release\.cairo\].*?inlining-strategy\s*=\s*)"
            rf'(?:\d+|"{AVOID_STRATEGY}")'
        )

        content = re.sub(pattern_release, rf"\g<1>{replacement_value}", content, flags=re.DOTALL)

        with open(self.scarb_toml_path, "w") as f:
            f.write(content)

        print(f"Updated root Scarb.toml")

        # Update perpetuals Scarb.toml
        with open(self.perpetuals_scarb_toml_path, "r") as f:
            content = f.read()

        # Update dev profile
        content = re.sub(pattern, rf"\g<1>{replacement_value}", content, flags=re.DOTALL)

        # Update release profile
        content = re.sub(pattern_release, rf"\g<1>{replacement_value}", content, flags=re.DOTALL)

        with open(self.perpetuals_scarb_toml_path, "w") as f:
            f.write(content)

        print(f"Updated perpetuals Scarb.toml")

    def build_contract(self) -> bool:
        """Build the contract using scarb build."""
        print("\nBuilding contract...")
        try:
            result = subprocess.run(
                ["scarb", "build"],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                timeout=300,
            )

            if result.returncode != 0:
                print(f"Build failed:\n{result.stderr}")
                return False

            print("Build successful")
            return True
        except subprocess.TimeoutExpired:
            print("Build timed out")
            return False
        except Exception as e:
            print(f"Build error: {e}")
            return False

    def get_contract_size(self) -> int:
        """Get the bytecode length from the compiled contract."""
        print("\nGetting contract size...")
        try:
            with open(self.bytecode_path, "r") as f:
                data = json.load(f)

            bytecode_length = len(data.get("bytecode", []))
            print(f"Contract size: {bytecode_length} felts")
            return bytecode_length
        except FileNotFoundError:
            print(f"Bytecode file not found: {self.bytecode_path}")
            return -1
        except Exception as e:
            print(f"Error reading bytecode: {e}")
            return -1

    def run_performance_test(self) -> int:
        """Run the performance test and extract l2_gas value."""
        print("\nRunning performance test...")
        try:
            result = subprocess.run(
                ["scarb", "test", "test_performance", "-p", "perpetuals"],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                timeout=600,
            )

            if result.returncode != 0:
                print(f"Test failed:\n{result.stderr}")
                return -1

            # Parse l2_gas from output
            # Example: [PASS] perpetuals::tests::performance_tests::
            # performance_tests::test_performance
            # (l1_gas: ~0, l1_data_gas: ~96, l2_gas: ~1411200)
            match = re.search(r"l2_gas:\s*~?(\d+)", result.stdout)
            if match:
                total_l2_gas = int(match.group(1))
                print(f"Total L2 Gas: {total_l2_gas:,}")
                return total_l2_gas
            else:
                print(f"Could not parse l2_gas from output:\n{result.stdout}")
                return -1
        except subprocess.TimeoutExpired:
            print("Test timed out")
            return -1
        except Exception as e:
            print(f"Test error: {e}")
            return -1

    def test_strategy(self, strategy_value) -> Tuple[int, int]:
        """Test a single inline-strategy value or 'avoid' to use current config."""
        self.update_inline_strategy(strategy_value)

        if not self.build_contract():
            return (-1, -1)

        contract_size = self.get_contract_size()
        l2_gas = self.run_performance_test()

        return (contract_size, l2_gas)

    def save_checkpoint(self, results: dict, current_index: int = None, total: int = None) -> None:
        """Save current results to checkpoint file."""
        checkpoint_data = {"timestamp": datetime.now().isoformat(), "results": results}
        try:
            with open(self.checkpoint_file, "w") as f:
                json.dump(checkpoint_data, f, indent=2)

            progress_str = ""
            if current_index is not None and total is not None:
                progress_pct = (current_index / total) * 100
                progress_str = f" ({current_index}/{total} = {progress_pct:.1f}%)"

            print(f"Checkpoint saved{progress_str}")
        except Exception as e:
            print(f"Warning: Could not save checkpoint: {e}")

    def test_multiple_strategies(self, strategy_values: List) -> dict:
        """Test multiple inline-strategy values and collect results."""
        results = {
            "strategies": [],
            "contract_sizes": [],
            "total_l2_gas": [],
            "l2_gas_per_settlement": [],
            "avoid_data": None,  # Store avoid baseline separately
        }

        # Performance test runs 12 settlements
        num_settlements = 12

        # This is an approximation of the overhead gas of the performance test.
        overhead_gas = TEST_OVERHEAD

        num_tests = len(strategy_values)

        for current_test, value in enumerate(strategy_values, start=1):
            # Handle "avoid" baseline test (uses current config without updating)
            if value == AVOID_STRATEGY:
                contract_size, l2_gas = self.test_strategy(AVOID_STRATEGY)

                if contract_size > 0 and l2_gas > 0:
                    total_l2_gas = l2_gas
                    l2_gas_per_settlement = (total_l2_gas - overhead_gas) / num_settlements

                    results["avoid_data"] = {
                        "contract_size": contract_size,
                        "total_l2_gas": total_l2_gas,
                        "l2_gas_per_settlement": l2_gas_per_settlement,
                        "label": "avoid",
                    }

                    print(
                        f"Baseline (avoid): Size={contract_size:,} felts, "
                        f"Total L2 Gas={total_l2_gas:,}, "
                        f"L2 Gas/Settlement={l2_gas_per_settlement:,.0f}"
                    )
                    self.save_checkpoint(results, current_test, num_tests)
                else:
                    print(f"Baseline test failed")
                continue

            contract_size, l2_gas = self.test_strategy(value)

            if contract_size > 0 and l2_gas > 0:
                total_l2_gas = l2_gas
                l2_gas_per_settlement = (total_l2_gas - overhead_gas) / num_settlements

                results["strategies"].append(value)
                results["contract_sizes"].append(contract_size)
                results["total_l2_gas"].append(total_l2_gas)
                results["l2_gas_per_settlement"].append(l2_gas_per_settlement)

                print(
                    f"Strategy {value}: Size={contract_size:,} felts, "
                    f"Total L2 Gas={total_l2_gas:,}, "
                    f"L2 Gas/Settlement={l2_gas_per_settlement:,.0f}"
                )

                # Save checkpoint after each successful test
                self.save_checkpoint(results, current_test, num_tests)
            else:
                print(f"Strategy {value}: Failed")

        return results

    def plot_results(self, results: dict, output_file: str = "inline_strategy_results.png") -> None:
        """Plot the results with dual y-axes."""
        if not results["strategies"]:
            print("No valid results to plot")
            return

        print(f"\nGenerating plot...")

        fig, ax1 = plt.subplots(figsize=(14, 7))

        # Plot contract size on left y-axis
        color1 = "tab:blue"
        ax1.set_xlabel("Inline Strategy Value", fontsize=12)
        ax1.set_ylabel("Contract Size (felts)", color=color1, fontsize=12)
        line1 = ax1.plot(
            results["strategies"],
            results["contract_sizes"],
            "o-",
            color=color1,
            linewidth=2,
            markersize=8,
            label="Contract Size",
        )
        ax1.tick_params(axis="y", labelcolor=color1)
        ax1.grid(True, alpha=0.3)

        # Create second y-axis for L2 Gas per Settlement
        ax2 = ax1.twinx()
        color2 = "tab:red"
        ax2.set_ylabel("L2 Gas per Settlement", color=color2, fontsize=12)
        line2 = ax2.plot(
            results["strategies"],
            results["l2_gas_per_settlement"],
            "s-",
            color=color2,
            linewidth=2,
            markersize=8,
            label="L2 Gas/Settlement",
        )
        ax2.tick_params(axis="y", labelcolor=color2)

        # Add baseline (avoid) info if present
        if results.get("avoid_data"):
            avoid = results["avoid_data"]

            # Baseline Contract Size in blue at top center
            ax1.text(
                0.50,
                0.97,
                f'Baseline Contract Size: {avoid["contract_size"]:,} felts',
                transform=ax1.transAxes,
                fontsize=12,
                verticalalignment="top",
                horizontalalignment="center",
                color=color1,
                fontweight="bold",
            )

            # Baseline L2 Gas/Settlement in red below contract size
            ax1.text(
                0.50,
                0.94,
                f"Baseline L2 Gas/Settlement: " f'{avoid["l2_gas_per_settlement"]:,.0f}',
                transform=ax1.transAxes,
                fontsize=12,
                verticalalignment="top",
                horizontalalignment="center",
                color=color2,
                fontweight="bold",
            )

        # Add title
        title = "Inline Strategy Impact on Contract Size and L2 Gas per Settlement"
        plt.title(title, fontsize=14, fontweight="bold", pad=20)

        # Combine legends - move to upper right
        lines_all = line1 + line2
        labels_all = ["Contract Size", "L2 Gas/Settlement"]
        ax1.legend(lines_all, labels_all, loc="upper right", fontsize=11, framealpha=0.9)

        # Format y-axis labels with commas
        ax1.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f"{int(x):,}"))
        ax2.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f"{int(x):,}"))

        # Adjust layout and save
        fig.tight_layout()
        output_path = self.repo_path / output_file
        plt.savefig(output_path, dpi=300, bbox_inches="tight")
        print(f"Plot saved to: {output_path}")

        # Also show the plot
        plt.show()

    def print_summary(self, results: dict) -> None:
        """Print a summary table of results."""
        print(f"\n{'='*140}")
        print("SUMMARY OF RESULTS")
        print(f"{'='*140}")

        # Print baseline (avoid) data if present
        if results.get("avoid_data"):
            avoid = results["avoid_data"]
            print(f'\n** BASELINE REFERENCE (inline-strategy="avoid") **')
            print(f"Contract Size: {avoid['contract_size']:,} felts")
            print(f"Total L2 Gas: {avoid['total_l2_gas']:,}")
            print(f"L2 Gas/Settlement: {avoid['l2_gas_per_settlement']:,.0f}")
            print(f"{'-'*140}\n")

        print(
            f"{'Strategy':<12} {'Contract Size':<20} {'Total L2 Gas':<25} "
            f"{'L2 Gas/Settlement':<25} {'Size Change':<20} "
            f"{'L2 Gas/Settle Change':<20}"
        )
        print(f"{'-'*140}")

        for i, strategy in enumerate(results["strategies"]):
            size = results["contract_sizes"][i]
            total_gas = results["total_l2_gas"][i]
            gas_per_settlement = results["l2_gas_per_settlement"][i]

            # Calculate changes relative to avoid_data if it exists
            if results.get("avoid_data"):
                avoid = results["avoid_data"]
                size_diff = size - avoid["contract_size"]
                gas_diff = gas_per_settlement - avoid["l2_gas_per_settlement"]
                size_pct = (size_diff / avoid["contract_size"]) * 100
                gas_pct = (
                    (gas_diff / avoid["l2_gas_per_settlement"]) * 100
                    if avoid["l2_gas_per_settlement"] != 0
                    else 0
                )
                size_change = f"{size_diff:+,} ({size_pct:+.1f}%)"
                gas_change = f"{gas_diff:+,.0f} ({gas_pct:+.1f}%)"
            else:
                # No avoid_data: show absolute change relative to first strategy
                if i == 0:
                    size_change = "-"
                    gas_change = "-"
                else:
                    size_diff = size - results["contract_sizes"][0]
                    gas_diff = gas_per_settlement - results["l2_gas_per_settlement"][0]
                    size_change = f"{size_diff:+,}"
                    gas_change = f"{gas_diff:+,.0f}"

            print(
                f"{strategy:<12} {size:>15,} felts  {total_gas:>20,}     "
                f"{gas_per_settlement:>20,.0f}     {size_change:<20} "
                f"{gas_change:<20}"
            )

        print(f"{'='*140}\n")


def main():
    parser = argparse.ArgumentParser(
        description=(
            "Test different inline-strategy values and measure their impact on "
            "contract size and L2 gas."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python ./scripts/test_inline_strategy.py 5 10 15 20 25 30
  python ./scripts/test_inline_strategy.py avoid 10 20 30 40 50
  python ./scripts/test_inline_strategy.py --plot-only
  python ./scripts/test_inline_strategy.py  # Uses default values
        """,
    )

    parser.add_argument(
        "strategy_values",
        nargs="*",
        help=f"Inline-strategy values to test (integers or '{AVOID_STRATEGY}'). "
        f"If not specified, defaults to '{AVOID_STRATEGY}' and range(0, 101, 5).",
    )

    parser.add_argument(
        "--plot-only",
        action="store_true",
        help="Plot results from checkpoint file without running tests",
    )

    args = parser.parse_args()

    if args.plot_only:
        print("Loading results from checkpoint file...")
        tester = InlineStrategyTester(repo_path=get_project_root())

        if not tester.checkpoint_file.exists():
            print(f"Checkpoint file not found: {tester.checkpoint_file}")
            print("Run tests first to generate checkpoint data.")
            sys.exit(1)

        try:
            with open(tester.checkpoint_file, "r") as f:
                checkpoint_data = json.load(f)
            results = checkpoint_data.get("results", {})

            if not results.get("strategies"):
                print("No test results found in checkpoint file")
                sys.exit(1)

            print(f"Loaded {len(results['strategies'])} test results")
            if results.get("avoid_data"):
                print("Baseline (avoid) data found")

        except Exception as e:
            print(f"Error loading checkpoint: {e}")
            sys.exit(1)
    else:
        # Parse strategy values
        if not args.strategy_values:
            # Default values: 'avoid' + range(0, 101, 5)
            strategy_values = [AVOID_STRATEGY] + list(range(0, 101, 5))
        else:
            strategy_values = []
            for arg in args.strategy_values:
                if arg.lower() == AVOID_STRATEGY:
                    strategy_values.append(AVOID_STRATEGY)
                else:
                    try:
                        strategy_values.append(int(arg))
                    except ValueError:
                        print(
                            f"Error: Invalid argument '{arg}'. "
                            f"Arguments must be integers or '{AVOID_STRATEGY}'"
                        )
                        sys.exit(1)

        print(f"Testing inline-strategy values: {strategy_values}")
        if AVOID_STRATEGY in strategy_values:
            print(f"Note: '{AVOID_STRATEGY}' will set inline-strategy to " f'"{AVOID_STRATEGY}"')
        print(f"Checkpoints will be saved to: inline_strategy_checkpoint.json\n")

        # Create tester and run tests
        tester = InlineStrategyTester(repo_path=get_project_root())
        results = tester.test_multiple_strategies(strategy_values)

    # Print summary and plot results
    if results["strategies"] or results["avoid_data"]:
        if not args.plot_only:
            print("ALL TESTS COMPLETED")

        tester.print_summary(results)
        tester.plot_results(results)

        if not args.plot_only:
            # Save final results
            final_results_file = tester.repo_path / "inline_strategy_final_results.json"
            with open(final_results_file, "w") as f:
                json.dump(results, f, indent=2)
            print(f"Final results saved to: {final_results_file}")
    else:
        print("No successful tests to report")
        sys.exit(1)


if __name__ == "__main__":
    main()
