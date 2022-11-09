#!/usr/bin/env bash

# exit immediately upon first error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

if [ -z "$1" ] || [ -z "$2" ]; then
  echo "expected an output directory"
  exit 1
else
  output="$1"
  cfg="$2"
fi

#
#   COLLECT OUTPUT
#

pushd "$output"

echo "[" > all.json
echo "[" > errors.json
echo "[" > timeouts.json

first=true
first_error=true
first_timeout=true

for rj in $(find . -name 'results.json' | sort); do
  if $first; then
    first=false
  else
    echo "," >> all.json
  fi

  quote_seed="$(jq '.seed' "$rj")"
  temp="${quote_seed%\"}"
  seed="${temp#\"}"

  npts="$(jq '.points' "$rj")"
  herbie_iters="$(jq '.iterations' "$rj")"

  # warn about errors and timeouts that will be filtered out

  errors="$(jq '.tests | map(select((.status == "error") or (.status == "crash")))' "$rj")"
  if [ "$errors" != "[]" ]; then
  	if $first_error; then
  	  first_error=false
  	else
  	  echo "," >> errors.json
  	fi
    echo "WARNING: filtering out errors in $rj on seed $seed"
    echo "$errors"
    echo "{ \"seed\" : $seed ," >> errors.json
    echo " \"errors\" :" >> errors.json
    echo "$errors" >> errors.json
    echo "}" >> errors.json
  fi

  timeouts="$(jq '.tests | map(select(.status == "timeout"))' "$rj")"
  if [ "$timeouts" != "[]" ]; then
  	if $first_timeout; then
  	  first_timeout=false
  	else
  	  echo "," >> timeouts.json
  	fi
    echo "WARNING: filtering out timeouts in $rj on seed $seed"
    echo "$timeouts"
    echo "{ \"seed\" : $seed ," >> timeouts.json
    echo " \"timeouts\" :" >> timeouts.json
    echo "$timeouts" >> timeouts.json
    echo " }" >> timeouts.json
  fi
  cat "$rj" \
    | jq --argjson SEED "$seed" \
         --argjson NPTS "$npts" \
         --argjson HERBIE_ITERS "$herbie_iters" \
         --arg CFG "$cfg" \
      '.tests | map(
         select(.status != "error") |
         select(.status != "timeout") |
         select(.status != "crash") |
         { "test" : .name
         , "input" : .input
         , "output" : .output
         , "output_parens" : (.output | [match("[(]"; "g")] | length)
         , "avg_bits_err_input": .start
         , "avg_bits_err_output": .end
         , "avg_bits_err_improve": (.start - .end)
         , "time": .time
         , "seed": $SEED
         , "npts": $NPTS
         , "herbie_iters": $HERBIE_ITERS
         , "config": $CFG
         })' \
    >> all.json
done
echo "]" >> all.json
echo "]" >> errors.json
echo "]" >> timeouts.json

# flatten array of array of results to an array
jq 'flatten' all.json > all.json.tmp
mv all.json.tmp all.json

jq 'flatten' errors.json > errors.json.tmp
mv errors.json.tmp errors.json

jq 'flatten' timeouts.json > timeouts.json.tmp
mv timeouts.json.tmp timeouts.json

popd
