#!/bin/bash

# Output format:
# 
# label <label>
# bench <num>
# <output>
# done

# label <type> <program-name>
function label ()
{
  local type=$1
  local program_name=$2
  echo "${type} -- ${program_name}"
}

# bench <label> <num_runs> <program...>
function bench ()
{
  local label=$1
  local num_runs=$2
  local program=${@:3}
  local os=$(uname)
  local time_flags
  if [[ ${os} == "Darwin" ]]; then
    time_flags="-l"
  else
    time_flags="-f'%e real %M'"
  fi

  for ((i = 0; i < $num_runs; i++)); do
    echo "label ${label}"
    echo "bench ${i}"
    /usr/bin/time ${time_flags} "${program}"
    echo "done"
  done
}

# baseline <args>
function baseline ()
{
  local compile="ocamlopt"
  ${compile} $@
}

# parc <args>
function parc ()
{
  local compile="$workdir/boot/ocamlrun $workdir/ocamlopt"
  ${compile} $@
}

# main <num_runs>
function main()
{
  local num_runs=$1
  local workdir=$(pwd)
  
  local srcdir=$workdir/my/parc/
  local programs=(
    "rbtree.ml"
    "rbtree_ck.ml"
    "deriv.ml"
    "nqueens.ml"
    "cfold.ml"
  )
  local srcs=("${programs[@]/#/"$srcdir"}")
  
  local baseflags="-S -O2 -g"
  local stdlibflag="-I $workdir/stdlib"
  local extra_flags

  for src in ${srcs[@]}; do
    local program=$(basename -- ${src} .ml)
    local output=$workdir/$program
  
    # baseline
    baseline ${baseflags} -o ${output} ${src}
    bench \
      "$(label "baseline" ${program})" \
      "${num_runs}" \
      "${output}"
  
    # parc
    extra_flags="-automated-refcounting"
    parc ${baseflags} ${stdlibflag} ${extra_flags} -o ${output} ${src}
    bench \
      "$(label "parc" ${program})" \
      "${num_runs}" \
      "${output}"

    # parc + drop specialization
    extra_flags="-automated-refcounting -drop-specialization"
    parc ${baseflags} ${stdlibflag} ${extra_flags} -o ${output} ${src}
    bench \
      "$(label "parc+drop-specialization" ${program})" \
      "${num_runs}" \
      "${output}"
  done
}

main 5
