"""
Kicks things off via

python -m nnreddit

"""

import os
import sys
import jsonrpyc

from .AuthenticatedReddit import AuthenticatedReddit

stdin = sys.stdin
stdout = sys.stdout
if __name__ != "__main__":
     sys.stdout = sys.stderr = open(os.devnull, "w")

jsonrpyc.RPC(target=AuthenticatedReddit(), stdin=stdin, stdout=stdout)
