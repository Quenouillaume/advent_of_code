import os
import sys
import requests
import time
from dotenv import load_dotenv, find_dotenv
from authlib.integrations.requests_client import OAuth2Session


load_dotenv()
session_id = os.getenv('AOC_SESSION')

def get_level(year, day):
    uri = f'https://adventofcode.com/{year}/day/{day}'
    response = requests.post(uri,cookies={'session': session_id})
    if "The first half of this puzzle is complete!" in response.text:
        return 2
    return 1



def submit(year, day, answer):
    level = get_level(year, day)

    print(f"Submitting solution for level {level} of {year}/{day}...")
    uri = f'https://adventofcode.com/{year}/day/{day}/answer'
    payload = {"level": str(level), "answer": str(answer)}
    response = requests.post(uri,cookies={'session': session_id}, data=payload)
    #print(response.text)
    if 'You gave an answer too recently' in response.text:
        print('VERDICT : TOO MANY REQUESTS')
    elif 'not the right answer' in response.text:
        if 'too low' in response.text:
            print('VERDICT : WRONG (TOO LOW)')
        elif 'too high' in response.text:
            print('VERDICT : WRONG (TOO HIGH)')
        else:
            print('VERDICT : WRONG (UNKNOWN)')
    elif 'seem to be solving the right level.' in response.text:
            print('VERDICT : INVALID LEVEL OR ALREADY COMPLETED')
    else:
        print('VERDICT : OK !')

if __name__ == "__main__":
    argc = len(sys.argv)
    if argc < 4:
        print("Too few arguments")
        exit()
    year = int(sys.argv[1])
    day = int(sys.argv[2])
    fn = sys.argv[3]
    with open(fn, 'r') as f:
        answer = f.read()
        if answer.isnumeric() and int(answer) <= 0:
            print(f"Test or no answer: {answer}")
        else:
            submit(year, day, answer)