#!/bin/bash

bazel build :main

while true; do
  output=$(./bazel-bin/main)

  if [[ $(echo $output | grep -c "BAD SUM") -eq 1 ]]; then
    echo "$output"
    exit 0
  fi
done
