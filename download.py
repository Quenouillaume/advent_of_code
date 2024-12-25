import os
import sys
import requests
import time
from dotenv import load_dotenv, find_dotenv
from authlib.integrations.requests_client import OAuth2Session


"""
Download input.

 `python3 download.py year day` to download corresponding day and
store it in [year]/input/day_[day].txt, if file does not already exist.

If no arguments are provided, current year and day will be used.
"""


year = None
load_dotenv()
session_id = os.getenv('AOC_SESSION')


# def get_test(day, stripped=True):
#     fn = f'{year}/day_{day}/test.txt'
#     res = None
#     f = open(fn, 'r')
#     res = f.read()
#     f.close()
#     lines = res.split('\n')
#     if stripped:
#         lines = [l.strip() for l in lines]
#     else:
#         lines = [l.replace('\n','') for l in lines]
#
#     if lines[-1] == '':
#         lines.pop()
#     return lines


def get_input(day, stripped=True, split=True):
    assert(year is not None)
    uri = f'https://adventofcode.com/{year}/day/{day}/input'
    fn = f'{year}/input/day_{day}.txt'
    res = None
    try:
        f = open(fn, 'r')
        res = f.read()
        f.close()
    except:
        print(f"Day {day} not found, downloading")
        r = requests.get(uri, cookies={'session': session_id})
        res = r.text
        if "Please don't repeatedly" in res:
            print("Not yet available")
            return None
        f = open(fn, 'w')
        f.write(res)
        f.close()

    if not split:
        return res
    lines = res.split('\n')

    if stripped:
        lines = [l.strip() for l in lines]
    else:
        lines = [l.replace('\n','') for l in lines]

    if lines[-1] == '':
        lines.pop()
    return lines


if __name__ == "__main__":
    argc = len(sys.argv)

    if argc == 1:
        year = time.localtime().tm_year
        day = time.localtime().tm_mday
    else:
        year = int(sys.argv[1])
        day = int(sys.argv[2])

    get_input(day, split=False)


