#!/usr/bin/env python3

# Perform some checks on the output of the Metrics plugin, but avoiding
# too many details which cause conflicts during merge of libc branches.
#
# This script expects a multiple of 5 arguments containing the expressions
# to compare for each 'run' of metrics: the first argument will be compared
# to the number of defined functions; the second argument to the number of
# specified functions; etc, until the 5th. Then, the 6th argument will
# correspond to the following run of metrics, until the 10th; and so on.
#
# This test reads stdin and expects to find the output of one or more metrics
# runs, which it then reads line by line, parses the relevant numbers,
# and compares them to the expressions passed as arguments.

import re
import sys

print("Checking libc metrics...", flush=True)

args = sys.argv[1:]

if len(args) < 1 or len(args) % 5 != 0:
    sys.exit(f"error: expected a non-zero multiple of 5 arguments, got {len(args) % 5}")

runs = []

title_re = re.compile(r"(\[metrics\] )?([a-zA-Z0-9'][a-zA-Z0-9' -]+)+ +\(([0-9]+)\)")

cur_run = 0
for run in range(0, len(args) // 5):
    r = {}
    r["Defined functions"] = args[5 * cur_run]
    r["Specified-only functions"] = args[5 * cur_run + 1]
    r["Undefined and unspecified functions"] = args[5 * cur_run + 2]
    r["'Extern' global variables"] = args[5 * cur_run + 3]
    r["Potential entry points"] = args[5 * cur_run + 4]
    cur_run += 1
    runs.append(r)


def check_condition(run, title, value):
    cond = run[title]
    del run[title]
    test = eval(value + cond)
    if not test:
        print(f"warning: expected {title} {cond}, but got {value}")
        return False
    return True


cur_run = 0
ok = True
for line in sys.stdin.readlines():
    m = title_re.match(line.strip())
    if m:
        title = m.group(2)
        value = m.group(3)
        ok = check_condition(runs[cur_run], title, value) and ok
        if runs[cur_run] == {}:
            cur_run += 1
            if cur_run >= len(runs):
                break

print("Finished checking libc metrics.")
