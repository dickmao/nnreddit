# -*- coding: utf-8 -*-

# The following is a derivative work of
# https://github.com/praw-dev/praw
# licensed under BSD 2-Clause "Simplified" License.

from betamax import Betamax
from betamax.cassette.cassette import Placeholder
import functools
import json
import logging
from six.moves.urllib.parse import parse_qs

__recordings__ = {}

def scrub(interaction, current_cassette):
    request = interaction.data.get('request') or {}
    response = interaction.data.get('response') or {}

    # Exit early if the request did not return 200 OK because that's the
    # only time we want to look for tokens
    if not response or response['status']['code'] != 200:
        return

    for what in [r for r in [request, response] if r]:
        auths = what['headers'].get('Authorization') or []
        for auth in auths:
            current_cassette.placeholders.append(
                Placeholder(placeholder='**********', replace=auth)
            )

        body_string = what['body']['string']
        try:
            dikt = json.loads(body_string)
        except:
            dikt = { k: v[0] for k,v in parse_qs(body_string).items() }
        for token in ['access_token', 'refresh_token']:
            if token in dikt:
                current_cassette.placeholders.append(
                    Placeholder(placeholder='**********', replace=dikt[token])
                )

with Betamax.configure() as config:
    config.cassette_library_dir = 'tests/cassettes'
    config.before_record(callback=scrub)

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

def recording_end(cassette=None):
    if cassette and cassette not in __recordings__:
        raise RuntimeError('Recording {} not in progress!'.format(cassette))

    if cassette is None:
        [c.__exit__() for c in __recordings__.values()]
    else:
        __recordings__[cassette].__exit__()
        del __recordings__[cassette]
