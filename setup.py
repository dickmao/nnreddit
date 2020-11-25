"""nnreddit setup.py."""

import re
from codecs import open
from os import path
from setuptools import setup


PACKAGE_NAME = "nnreddit"
HERE = path.abspath(path.dirname(__file__))

# https://packaging.python.org/guides/single-sourcing-package-version/#single-sourcing-the-version
# I chose method #4 as it touts multi-language access, i.e., elisp and python
# Method #1a via setup.cfg is cleanest, though.
with open(path.join('nnreddit', 'VERSION')) as version_file:
    version = version_file.read().strip()

setup(
    name=PACKAGE_NAME,
    author="dickmao",
    description="PRAW nnreddit backend",
    license="GPLv3",
    packages=[PACKAGE_NAME],
    version=version,
    package_data={
        "nnreddit": ["templates/*"],
    },
    install_requires=[
        "jsonrpyc>=1.1.0",
        "rtv>=1.26.0",
#        "praw<=6.3.1",
        "praw>=6.2",
    ],
)
