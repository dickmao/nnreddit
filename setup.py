"""nnreddit setup.py."""

import re
from codecs import open
from os import path
from setuptools import setup


PACKAGE_NAME = "nnreddit"
HERE = path.abspath(path.dirname(__file__))

setup(
    name=PACKAGE_NAME,
    author="dickmao",
    description="PRAW nnreddit backend",
    license="GPLv3",
    packages=[PACKAGE_NAME],
    version="0.1.0",
    package_data={
        "nnreddit": ["templates/*"],
    },
    install_requires=[
        "jsonrpyc",
        "rtv",
        "praw",
    ],
)
