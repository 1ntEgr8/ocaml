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

BASELINE = "GC"


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
    plt.rcParams.update({
        "text.usetex": True,
        "font.family": "Helvetica"
    })
    colors = ["#E1BC29", "#E15554", "#4D9DE0", "#3BB273", "#7768AE"]

    def plot(gp, ylabel, output, legend=True):
        ax = gp.unstack().plot.bar(
            color = colors,
            zorder = 2,
            edgecolor = "black",
            linewidth=0.5
        )
        ax.set_xlabel("")
        ax.set_xticklabels(ax.get_xticklabels(), rotation=0)
        ax.set_ylabel(ylabel,fontsize=14)
        ax.axhline(y=1.0,color="black",zorder=1,linewidth=0.5)
        for container in ax.containers:
            ax.bar_label(container, fmt="%.2f", rotation="vertical", padding=10)
        if not legend:
            ax.get_legend().remove()
        plt.savefig(output_dir / output)
    
    df["Backend"] = df["compiler"]
    gp = df.groupby(["benchmark", "Backend"]).mean(numeric_only=True)
    gp_time = gp["time"] / gp.unstack()["time"][BASELINE]
    gp_rss = gp["rss"] / gp.unstack()["rss"][BASELINE]

    plot(gp_time, "Relative time (lower is better)", "time.pdf") 
    plot(gp_rss, "Relative rss (lower is better)", "rss.pdf", legend=False) 

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-raw", action="append", default=[], type=Path)
    parser.add_argument("--input-csv", default=[], type=Path)
    parser.add_argument("--output-dir", type=Path, help="Output directory")

    args = parser.parse_args()

    if args.input_csv:
        df = pd.read_csv(args.input_csv)
    else:
        df = parse_input(args.input_raw)

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
