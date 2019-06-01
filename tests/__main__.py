"""
Kicks things off via

python -m nnreddit

"""

import os
import sys
import jsonrpyc

os.environ["XDG_DATA_HOME"] = os.path.join(os.path.dirname(__file__), 'share')
try:
    os.makedirs(os.environ["XDG_DATA_HOME"])
except OSError:
    if not os.path.isdir(os.environ["XDG_DATA_HOME"]):
        raise

from nnreddit.AuthenticatedReddit import AuthenticatedReddit

stdin = sys.stdin
stdout = sys.stdout
if __name__ != "__main__":
    sys.stdout = sys.stderr = open(os.devnull, "w")

logdir = os.path.join(os.path.dirname(__file__), 'log')
try:
    os.makedirs(logdir)
except OSError:
    if not os.path.isdir(logdir):
        raise
jsonrpyc.RPC(target=AuthenticatedReddit(log_prefix=os.path.join(logdir, 'test_py.'),
                                        check_for_updates=False),
             stdin=stdin, stdout=stdout)
