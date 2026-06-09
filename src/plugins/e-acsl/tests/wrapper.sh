#!/bin/bash
# Wrapper script to compile a test file and execute the resulting binary

# Base dir of this script
BASEDIR="$(realpath `dirname $0`)"

help() {
  printf "wrapper.sh - run e-acsl-gcc and the resulting binary to test E-ACSL
Usage: $0 framac_exe result_dir test_name test_file output_name opts fc_opts exit_code filter_cmd
Args:
  framac_exe    @frama-c-exe@ in test_config
  result_dir    @PTEST_RESULT_DIR@ in test_config
  test_name     @PTEST_NAME@ in test_config
  test_file     @PTEST_FILE@ in test_config
  output_name   Output file given to LOG in test_config
  opts          Options for e-acsl-gcc
  fc_opts       Options for Frama-C
  exit_code     Expected exit code of resulting binary
  filter_cmd    Command to filter the output of the resulting binary
" >&2
  exit 1
}

if [[ $# -ne 9 ]]; then
  printf "Error: wrong number of arguments: 9 expected, got $#\n\n" >&2
  help
fi

# Name the arguments of the script
framac_exe=$1
result_dir=$2
test_name=$3
test_file=$4
output_name=$5
opts=$6
fc_opts=$7
expected_exit_code=$8
filter_cmd=$9

# Derive output logs from the arguments
out_log=$result_dir/$test_name.res.log
err_log=$result_dir/$test_name.err.log
exec_out_log=$result_dir/$test_name.exec_out.log
exec_err_log=$result_dir/$test_name.exec_err.log
output_log=$result_dir/$output_name

# Compile the test file
e-acsl-gcc -I $framac_exe \
  $opts \
  --frama-c-extra="$fc_opts" \
  -o $result_dir/$test_name.gcc.c \
  -O $result_dir/$test_name \
  $test_file \
  1>$out_log \
  2>$err_log

# Check compilation return code and exit script in case of error
error_code=$?
if [[ $error_code -ne 0 ]]; then
  printf "Error while executing e-acsl-gcc\n" | tee $output_log
  printf "\nSTDOUT:\n" | tee -a $output_log
  cat $out_log | tee -a $output_log
  printf "\nSTDERR:\n" | tee -a $output_log
  cat $err_log | tee -a $output_log
  exit $error_code
fi

# Execute the compiled test file
# (Run in a grouping expression { } to be able to capture shell messages like
#  "Aborted")
{ $result_dir/$test_name.e-acsl; } 1>$exec_out_log 2>$exec_err_log
error_code=$?

# Check execution return code and exit script in case of error
if [[ $error_code -ne $expected_exit_code ]]; then
  printf "Error while executing $result_dir/$test_name.e-acsl\n" | tee $output_log
  printf "Expected exit code $expected_exit_code, got $error_code\n" | tee -a $output_log
  printf "\nSTDOUT:\n" | tee -a $output_log
  cat $exec_out_log | tee -a $output_log
  printf "\nSTDERR:\n" | tee -a $output_log
  cat $exec_err_log | tee -a $output_log
  exit 1 # Do not return directly error_code as it can be 0
fi

# No error while executing the script, filter stderr before saving it to the
# output log

## Create temporary files
tmp_filter_input=$(mktemp) || (printf "unable to create temp file\n" | tee $output_log && exit 1)
tmp_filter_output=$(mktemp) || (printf "unable to create temp file\n" | tee $output_log && exit 1)
cp $exec_err_log $tmp_filter_input

## Split the filter command on character | to extract the subcommands and apply
## the filters one at a time
IFS='|' read -ra filters <<< "$filter_cmd"
for filter in "${filters[@]}"; do
  cat $tmp_filter_input | $filter > $tmp_filter_output
  error_code=$?
  if [[ $error_code -ne 0 ]]; then
    printf "Error while filtering output with command '$filter'\n" | tee $output_log
    printf "\nFILTER INPUT:\n" | tee -a $output_log
    cat $tmp_filter_input | tee -a $output_log
    printf "\nFILTER OUTPUT:\n" | tee -a $output_log
    cat $tmp_filter_output | tee -a $output_log
    exit $error_code
  fi
  cp $tmp_filter_output $tmp_filter_input
done

# Filter messages from this script because they are localized and appear anyway
# if the test fails.
cat $tmp_filter_input | sed -e "s/^\.\.\/\.\.\/wrapper\.sh.*//g" > $tmp_filter_output

## Filtering done, copy output to the output log and remove temporary files
cp $tmp_filter_output $output_log
rm $tmp_filter_input
rm $tmp_filter_output

exit 0
