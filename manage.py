import os
import sys
import requests
import time
from dotenv import load_dotenv, find_dotenv
from authlib.integrations.requests_client import OAuth2Session


load_dotenv()
session_id = os.getenv('AOC_SESSION')

prelude = """
open Advent_of_code;;
let test = false
let data = get_input {year} {day}

let day_{day} (data: string list) =
    0

let main () =
    let sol = if test then -1 else day_{day} data in

    if sol > 0 then begin
        print_int sol; print_newline ();
        write_solution sol
    end	else if sol = 0 then
        print_endline "Solution found is zero...\n"
    else
        print_endline "Test"

let _ = main ()"""


def create_ml(year):
    for i in range(25):
        print("Creating", f"{year}/day_{i+1}.ml")
        with open(f"{year}/day_{i+1}.ml", "w") as f:
            f.write(prelude.format(year=year, day=i+1))

if __name__ == "__main__":
    argc = len(sys.argv)
    if argc == 3 and sys.argv[1] == "create":
        year = int(sys.argv[2])
        create_ml(year)
        exit()

    if argc == 1:
        year = time.localtime().tm_year
        day = time.localtime().tm_mday
    else:
        year = int(sys.argv[1])
        day = int(sys.argv[2])

    print(year, day)
    print("Compiling...")
    os.system(f"ocamlc unix.cma advent_of_code.ml {year}/day_{day}.ml")
    print("Executing...")
    os.system("./a.out")
    print("Submitting...")
    os.system(f"python3 submit.py {year} {day} bla.txt")

