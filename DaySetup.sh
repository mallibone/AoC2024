#!/bin/bash

# Check if an argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <day_number>"
  exit 1
fi

# Format the day number to two digits
x=$(printf "%02d" "$1")

# Define file paths
input_dir="Input"
test_file="${input_dir}/TestDay${x}.txt"
day_file="${input_dir}/Day${x}.txt"
scratchpad_src="Scratchpad_day00.fsx"
scratchpad_dest="Scratchpad_day${x}.fsx"

# Check if files already exist
if [ -e "$test_file" ] || [ -e "$day_file" ] || [ -e "$scratchpad_dest" ]; then
  echo "Warning: One or more files already exist. No action taken."
  exit 1
fi

# Create TestDayx.txt and Dayx.txt
touch "$test_file" "$day_file"

# Copy and modify the scratchpad file
cp "$scratchpad_src" "$scratchpad_dest"
sed -i '' "s/20/$1/g" "$scratchpad_dest"