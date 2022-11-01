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

# configuration
HERBIE_DIR=herbie/
HERBIE_COMMIT=2ac0716ca93715987f834f39a4d569358567faa5
OUTPUT_DIR=output2/
BENCH_DIR=ruler-bench/
SEEDS="seeds.txt"

# set timeout
if [ -z "$SEED_COUNT" ]; then
  SEED_COUNT=10
fi

# Install Herbie
BUILD_DIR=$HERBIE_DIR HERBIE_COMMIT=$HERBIE_COMMIT bash ./install.sh

# Form benchmark suite
mkdir -p $BENCH_DIR
cp -r "$HERBIE_DIR/bench/hamming" $BENCH_DIR
cp -r "$HERBIE_DIR/bench/pbrt.fpcore" $BENCH_DIR

# ASSUMING rules under rulesets/
# Convert Ruler rules to Herbie format
racket ruler-to-herbie.rkt --tags arithmetic \
  empty-rules.rkt rulesets/slide-rule.json slide-rule-rules.rkt
racket ruler-to-herbie.rkt --tags arithmetic --old-format \
  empty-rules.rkt rulesets/oopsla21.json oopsla21-rules.rkt

# prepare seeds
if [ -f "$SEEDS" ]; then
  echo "Found seeds file"
else
  echo "Created seeds file"
  shuf -i 1-65535 -n $SEED_COUNT | sort -n > $SEEDS
fi

# Run Herbie
mkdir -p $OUTPUT_DIR

# Run with current rules
echo "Running Pareto-Herbie with current ruleset"
cp default-rules.rkt $HERBIE_DIR/src/syntax/rules.rkt
HERBIE_DIR=$HERBIE_DIR bash run.sh $SEEDS $BENCH_DIR "$OUTPUT_DIR/main"

# Run with SlideRule rules
echo "Running Pareto-Herbie with SlideRule ruleset"
cp slide-rule-rules.rkt $HERBIE_DIR/src/syntax/rules.rkt
HERBIE_DIR=$HERBIE_DIR bash run.sh $SEEDS $BENCH_DIR "$OUTPUT_DIR/slide-rule"

# Run with OOPSLA21 rules
echo "Running Pareto-Herbie with OOPSLA 21 ruleset"
cp oopsla21-rules.rkt $HERBIE_DIR/src/syntax/rules.rkt
HERBIE_DIR=$HERBIE_DIR bash run.sh $SEEDS $BENCH_DIR "$OUTPUT_DIR/oopsla21"

# Plot results

bash plot-individual.sh $SEEDS $OUTPUT_DIR
bash seed-variance.sh "$OUTPUT_DIR/main" "main"
bash seed-variance.sh "$OUTPUT_DIR/slide-rule" "slide-rule"
bash seed-variance.sh "$OUTPUT_DIR/oopsla21" "oopsla21"
bash plot-old.sh "$OUTPUT_DIR"
