import argparse
import platform
import time
import csv
import fileinput
import pandas as pd
import matplotlib.pyplot as plt

from pathlib import Path
from typing import List

# Parse output
# Generate two artifacts:
# - csv
# - plot


def parse_input(files: List[Path]) -> pd.DataFrame:
    """
    Input format:

        label <label>
        bench <num>
        <output>
        done
    """
    columns = ["benchmark", "compiler", "run number", "time", "rss"]
    df = pd.DataFrame(columns=columns)

    row = {}
    with fileinput.input(files=files, encoding="utf-8") as f:
        while True:
            try:
                line = next(f)
                if line.startswith("done"):
                    row_df = pd.DataFrame([row])
                    df = pd.concat([df, row_df])
                    row = {}
                elif line.startswith("label"):
                    # format: label <compiler> -- <benchmark>
                    data = line.split()
                    row["benchmark"] = data[3]
                    row["compiler"] = data[1]
                elif line.startswith("bench"):
                    # format: bench <num>
                    data = line.split()
                    row["run number"] = int(data[1])
                else:
                    # try to parse time and memory
                    data = line.split()
                    if len(data) > 1 and data[1] == "real":
                        row["time"] = float(data[0])
                        if platform.system() == "Darwin":
                            line = next(f)
                            data = line.split()
                            row["rss"] = float(data[0])
                        else:
                            row["rss"] = float(data[2])
                    else:
                        # ignore line
                        pass
            except StopIteration:
                break
    return df

def make_plots(df: pd.DataFrame, output_dir: Path):
    gp = df.groupby(["benchmark", "compiler"]).mean(numeric_only=True)
    
    # Make time plot
    gp_time = gp["time"] / gp.unstack()["time"]["baseline"]
    ax = gp_time.unstack().plot.bar(title="Relative execution time")
    for container in ax.containers:
        ax.bar_label(container, fmt="%.2f")
    plt.savefig(output_dir / "time.png")

    # Make rss plot
    gp_rss = gp["rss"] / gp.unstack()["rss"]["baseline"]
    ax = gp_rss.unstack().plot.bar(title="Relative peak working set")
    for container in ax.containers:
        ax.bar_label(container, fmt="%.2f")
    plt.savefig(output_dir / "rss.png")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-file", action="append", default=[], type=Path)
    parser.add_argument("--output-dir", type=Path, help="Output directory")

    args = parser.parse_args()
    df = parse_input(args.input_file)
    
    if args.output_dir:
        if not args.output_dir.exists():
            args.output_dir.mkdir(parents=True)

        # Output raw csv
        df.to_csv(args.output_dir / "results.csv")
        
        # Output plots
        make_plots(df, args.output_dir)
    else:
        print(df)

if __name__ == "__main__":
    main()
