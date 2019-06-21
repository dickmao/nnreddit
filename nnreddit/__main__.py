"""
Kicks things off via

python -m nnreddit

"""

import os
import sys
import jsonrpyc
import argparse

from .authenticated_reddit import authenticated_reddit

parser = argparse.ArgumentParser()
parser.add_argument("--log", help="log filename")
args  = parser.parse_args()

stdin = sys.stdin
stdout = sys.stdout
if __name__ != "__main__":
     sys.stdout = sys.stderr = open(os.devnull, "w")

jsonrpyc.RPC(target=authenticated_reddit(check_for_updates=False,
                                         log_prefix=args.log),
             stdin=stdin, stdout=stdout)
