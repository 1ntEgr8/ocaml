# Perceus benchmarks

## Running the benchmarks

Pre-reqs:
- Python >= 3.8, with [pandas](https://pandas.pydata.org/) and [matplotlib](https://matplotlib.org/) installed

**All commands must be run from the root directory of the OCaml source**

```bash
# Build the source
$ make coldstart
$ make coreall opt-core

# Run the benchmarks
$ bash my/parc/bench.sh 2>&1 | tee /dev/tty | python3 my/parc/summarize.py --output-dir "_bench_out"

# View results
$ ls _bench_out
```
