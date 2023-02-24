import platform
import time
import csv
import fileinput
import pandas as pd

# Parse output
# Generate two artifacts:
# - csv
# - plot


def parse_input() -> pd.DataFrame:
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
    with fileinput.input(encoding="utf-8") as f:
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
                            row["rss"] = int(data[0])
                        else:
                            row["rss"] = int(data[2])
                    else:
                        # ignore line
                        pass
            except StopIteration:
                break
    return df

def main():
    data = parse_input()
    print(data)

if __name__ == "__main__":
    main()
