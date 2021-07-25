#!/usr/bin/env python3

import os
import sys
import difflib
from subprocess import Popen, PIPE, DEVNULL

HEADER = '\033[95m'
OKBLUE = '\033[94m'
OKCYAN = '\033[96m'
OKGREEN = '\033[92m'
WARNING = '\033[93m'
FAIL = '\033[91m'
ENDC = '\033[0m'
BOLD = '\033[1m'
UNDERLINE = '\033[4m'

test_dir = "./tests/"

failed = []

for filename in os.listdir(test_dir):
    if filename.endswith(".lang"):
        program_path = os.path.join(test_dir, filename)
        expected_output_path = program_path.replace(".lang", "_expected.txt")
        print(f"{OKCYAN}Testing {program_path}{ENDC}")

        process = Popen(["./build/compiler", program_path], stdout=DEVNULL, stderr=PIPE)
        (output, err) = process.communicate()
        exit_code = process.wait()
        if exit_code != 0: 
            failed.append(program_path)
            print(f"{WARNING}Compilation error:{ENDC}")
            sys.stdout.write(err.decode("utf-8"))
            continue

        process = Popen(["gcc", "./main.o"], stdout=DEVNULL, stderr=PIPE)
        (output, err) = process.communicate()
        exit_code = process.wait()
        if exit_code != 0:
            failed.append(program_path)
            print(f"{WARNING}Linking error:{ENDC}")
            sys.stdout.write(err.decode("utf-8"))
            continue

        else:
            try:
                with open(expected_output_path, 'rb') as f:
                    process = Popen(["./a.out"], stdout=PIPE)
                    (output, err) = process.communicate()
                    exit_code = process.wait()
                    
                    diff = difflib.ndiff(
                            f.read().decode("utf-8").splitlines(),
                            output.decode("utf-8").splitlines())
                    difflist = list(diff)

                    for l in difflist:
                        if l.startswith("-") or l.startswith("+"):
                            print(f"{WARNING}Unexpected output from test program:{ENDC}")
                            print("\n".join(difflist))
                            failed.append(program_path)
                            break
            except FileNotFoundError:
                print(f"{WARNING}Failed to open expected output file {expected_output_path}{ENDC}")
                failed.append(program_path)
            

if len(failed) > 0:
    print(f"{FAIL}{BOLD}Tests failed:{ENDC}")
    for path in failed:
        print(f"> {path}")
    exit(1)
else:
    print(f"{OKGREEN}{BOLD}Tests succeeded{ENDC}")
