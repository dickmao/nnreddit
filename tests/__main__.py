"""
Kicks things off via

python -m nnreddit

"""

import os
import sys
import jsonrpyc
import argparse
from tempfile import mkstemp

parser = argparse.ArgumentParser()
parser.add_argument("--token-file", help="refresh token filename")
args = parser.parse_args()

os.environ["XDG_DATA_HOME"] = os.path.join(os.path.dirname(__file__), 'share')
try:
    os.makedirs(os.environ["XDG_DATA_HOME"])
except OSError:
    if not os.path.isdir(os.environ["XDG_DATA_HOME"]):
        raise

from nnreddit.authenticated_reddit import AuthenticatedReddit
from rtv.config import TOKEN

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

super_secret = os.path.join(os.path.dirname(TOKEN), 'super-secret-refresh-token')
kwargs = { 'token_file': args.token_file if args.token_file else (super_secret if os.path.exists(super_secret) else TOKEN),
           'history_file': mkstemp(dir='/var/tmp')[1],
}
jsonrpyc.RPC(target=AuthenticatedReddit(log_prefix=os.path.join(logdir, 'test_py.'),
                                        decode_html_entities=False,
                                        disable_update_check=True,
                                        check_for_updates=False,
                                        **kwargs),
             stdin=stdin, stdout=stdout)
