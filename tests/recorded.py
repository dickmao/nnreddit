# -*- coding: utf-8 -*-

# The following is a derivative work of
# https://github.com/praw-dev/praw
# licensed under BSD 2-Clause "Simplified" License.

from betamax import Betamax
import functools

__recordings__ = {}

with Betamax.configure() as config:
    config.cassette_library_dir = 'tests/cassettes'

def recorded(func):
    """Intercept point for Betamax.  As a decorator for an AuthenticatedReddit method, it disallowed reentrant calls to that method under record_mode: once."""
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        reddit = args[0]
        http = reddit._core._requestor._http

        # Disable response compression in order to see the response bodies in
        # the betamax cassettes.
        http.headers["Accept-Encoding"] = "identity"

        with Betamax(http).use_cassette(func.__name__):
            return func(*args, **kwargs)
    return wrapper

def recording_begin(reddit, cassette):
    if cassette in __recordings__:
        raise RuntimeError('Recording {} already in progress!'.format(cassette))

    http = reddit._core._requestor._http

    # what praw does to prevent compression obscuring response bodies
    http.headers["Accept-Encoding"] = "identity"

    __recordings__[cassette] = Betamax(http).use_cassette(cassette).__enter__()

def recording_end(cassette):
    if cassette not in __recordings__:
        raise RuntimeError('Recording {} not in progress!'.format(cassette))

    __recordings__[cassette].__exit__()
    del __recordings__[cassette]

def recording_end(cassette=None):
    if cassette and cassette not in __recordings__:
        raise RuntimeError('Recording {} not in progress!'.format(cassette))

    if not cassette:
        [c.__exit__() for c in __recordings__.values()]
    else:
        __recordings__[cassette].__exit__()
        del __recordings__[cassette]
