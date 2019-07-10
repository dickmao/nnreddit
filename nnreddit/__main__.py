"""
Kicks things off via

python -m nnreddit

"""

import os
import sys
import jsonrpyc
import argparse

from .authenticated_reddit import AuthenticatedReddit

parser = argparse.ArgumentParser()
parser.add_argument("--log", help="log filename")
parser.add_argument("--localhost", help="ip or hostname of localhost", default='127.0.0.1')
args = parser.parse_args()

stdin = sys.stdin
stdout = sys.stdout
if __name__ != "__main__":
    sys.stdout = sys.stderr = open(os.devnull, "w")

jsonrpyc.RPC(target=AuthenticatedReddit(check_for_updates=False,
                                        log_prefix=args.log,
                                        localhost=args.localhost),
             stdin=stdin, stdout=stdout)
