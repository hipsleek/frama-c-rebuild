#!/usr/bin/env python3
# -*- coding: utf-8 -*-
##########################################################################
#                                                                        #
#  SPDX-License-Identifier LGPL-2.1                                      #
#  Copyright (C)                                                         #
#  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  #
#                                                                        #
##########################################################################

import re
import sys
import subprocess
import time
import os
import signal
import argparse
import uuid
import csv
import typing
from pathlib import Path

import frama_c_results
import results_display
import benchmark_database


class OperationException(Exception):
    pass


def build_make_environment(framac):
    if framac is None:
        env = {**os.environ}
        args = []
    else:
        env = { **os.environ,  "PATH" : f"{framac}/bin:{os.environ['PATH']}" }
        args = [
            f"FRAMAC_BIN={framac}/bin",
            f"FRAMAC={framac}/bin/frama-c"
        ]
    return env, args


def list_targets(dir):
    if not os.path.isdir(dir):
        raise OperationException(f"target is not a directory: {dir}")

    env, args = build_make_environment(framac)
    res = subprocess.run(
        ["make", "--directory", dir, "--quiet", "display-targets"] + args,
        env=env,
        stdout=subprocess.PIPE,
        encoding="ascii",
    )
    targets = res.stdout.split()
    res = []
    for target in targets:
        if target.endswith(".eva") or target.endswith(".parse"):
            res += [f"{dir}/{target}"]
        else:
            res += list_targets(target)
    return res


def clone_frama_c(clonedir, hash):
    print("Cloning Frama-C", hash, "...")
    try:
        clone_cmd = Path(__file__).parent / "clone.sh"
        res = subprocess.run(
            [clone_cmd, "--clone-dir", clonedir, hash],
            stdout=subprocess.PIPE,
            encoding="ascii",
            check=True)
        return res.stdout.strip() + "/build"
    except subprocess.CalledProcessError as e:
        raise OperationException(
            f"Cannot clone repository. Try to manually remove the broken clone"
            f"in {clonedir}\n{e.output}") from e

def run_make(framac, benchmark_tag=None):
    args = ["make", "--keep-going", "all"]
    env, var_args = build_make_environment(framac)
    if benchmark_tag is None:
        args += ["-j", str(os.cpu_count())]
    else:
        args += ["BENCHMARK=" + benchmark_tag]
    return subprocess.Popen(
        args + var_args,
        env=env,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
        preexec_fn=os.setsid,
    )


def terminate_process(process):
    if process is None:
        return b""
    else:
        try:
            os.killpg(os.getpgid(process.pid), signal.SIGTERM)
            pass
        except ProcessLookupError:
            pass
        output, errors = process.communicate()
        return errors


def smart_rename(target):
    target = re.sub(r"^\./", "", target)
    target = re.sub(r"main\.eva$", "", target)
    target = re.sub(r"\.eva$", "", target)
    target = re.sub(r"\.frama-c/", "", target)
    return target


def is_running(target):
    return os.path.isfile(target + "/running")


def poll_results(targets, benchmark_tag):
    results = []
    for target in targets:
        filename = target + "/stats.txt"
        result = frama_c_results.read(filename)
        result["target"] = target
        result["target_name"] = smart_rename(target)
        result["is_running"] = is_running(target)
        result["up_to_date"] = benchmark_tag is None or benchmark_tag == result["benchmark_tag"]
        results.append(result)
    return results


def dump_results_csv(results, path):
    with open(path, 'w', newline='') as file:
        fieldnames = [
            "target_name", "timestamp",
            "sem_reach_fun", "syn_reach_fun", "total_fun",
            "sem_reach_stmt", "syn_reach_stmt",
            "alarms", "warnings", "coverage",
            "user_time", "memory"]
        writer = csv.DictWriter(
            file,
            fieldnames=fieldnames,
            extrasaction='ignore')
        writer.writeheader()
        writer.writerows(results)


def run_analyses(display, database, framac, benchmark_tag):
    results = []
    targets = list_targets(".")
    process = run_make(framac, benchmark_tag)
    errors = b""
    next_poll = time.time()

    def update():
        nonlocal display, database, targets, benchmark_tag, results
        results = poll_results(targets, benchmark_tag)
        if not database is None:
            database.update(results)
        display.needs_update = True

    try:
        while process.poll() is None:
            if time.time() >= next_poll:
                update()
                next_poll = time.time() + 2.0
            display.process_inputs()
            if display.needs_update:
                display.print_table(results)
            time.sleep(0.05)
        update()
    except (KeyboardInterrupt, results_display.UserExitRequest):
        print("Analyzes interrupted by user.")
    except Exception as e:
        # terminate_process below is somehow blocking the exception printing
        errors += bytearray(str(e), "ascii")
        raise e
    finally:
        errors += terminate_process(process)
    return results, errors


parser = argparse.ArgumentParser(
    description="Run analyses and summarize the results. Must be run in a "
                "directory with a Makefile having two rules: 'all', a target "
                "that runs the analysis, and 'display-targets', the target that "
                "lists the built results.")
parser.add_argument("rev", nargs="?", metavar="REVISION",
    help="a Frama-C revision to use for analyses (default: use the "
        "default configuration for Frama-C)")
parser.add_argument("-b", "--benchmark",
    action="store_true",
    help="sets benchmark mode: do not run analyses in parallel and rerun all "
        "analyses")
parser.add_argument("-v", "--vs",
    action="store", metavar="REVISION", default="master",
    help="a revision to compare the results to")
parser.add_argument("-c", "--comment",
    action="store", metavar="COMMENT",
    help="when benchmarking, add this comment inside the database")
parser.add_argument("-p", "--repository-path",
    action="store", metavar="PATH",
    help="don't clone Frama-C, use this git repository instead")
parser.add_argument("-o", "--output-csv",
    action="store", metavar="PATH", type=Path,
    help="output the results to the given CSV file")

errors = b""

try:
    args = parser.parse_args()

    if args.repository_path is None:
        if args.rev is None:
            gitdir = None
            framac = None
        else:
            clonedir = "./frama-c-clones"
            gitdir = clonedir + "/frama-c.git"
            framac = clone_frama_c(clonedir, args.rev)
    else:
        framac = os.path.abspath(args.repository_path)
        gitdir = framac

    benchmark_tag: typing.Optional[str]
    if args.benchmark:
        benchmark_tag = str(uuid.uuid1())
        print("Running benchmarks with benchmark tag", benchmark_tag, "...")
    else:
        benchmark_tag = None
        print("Running analyses ...")

    benchmark_comment = args.comment

    if gitdir is None:
        database = None
    else:
        database = benchmark_database.Database(benchmark_tag, benchmark_comment,
            gitdir, args.rev, args.vs)

    results,errors = results_display.wrapper(run_analyses, database, framac,
        benchmark_tag, curses=True)

    print("Results:\n")
    results_display.PlainDisplay().print_table(results)

    if args.output_csv is not None:
        dump_results_csv(results, args.output_csv)

except OperationException as e:
    errors += bytearray(str(e), "ascii")

sys.stderr.buffer.write(errors + b"\n")
