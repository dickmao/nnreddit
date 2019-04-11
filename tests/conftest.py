# -*- coding: utf-8 -*-

# The following is a derivative work of
# https://github.com/michael-lazar/rtv
# whose copyright and license are copied here:

# The MIT License (MIT)
#
# Copyright (c) 2015 michael-lazar
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from __future__ import unicode_literals

import os
import logging
from functools import partial

import pytest
from vcr import VCR
from six.moves.urllib.parse import urlparse, parse_qs
from tempfile import mkstemp

os.environ["XDG_DATA_HOME"] = os.path.join(os.path.dirname(__file__), 'share')
try:
    os.makedirs(os.environ["XDG_DATA_HOME"])
except OSError:
    if not os.path.isdir(os.environ["XDG_DATA_HOME"]):
        raise

from nnreddit.AuthenticatedReddit import AuthenticatedReddit
from rtv.config import TOKEN

try:
    from unittest import mock
except ImportError:
    import mock

# Turn on autospec by default for convenience
patch = partial(mock.patch, autospec=True)

# Turn on logging, but disable vcr from spamming
logging.basicConfig(
    level=logging.DEBUG,
    format='%(asctime)s:%(levelname)s:%(filename)s:%(lineno)d:%(message)s')
for name in ['vcr.matchers', 'vcr.stubs']:
    logging.getLogger(name).disabled = True

def pytest_addoption(parser):
    parser.addoption('--record-mode', dest='record_mode', default='none')
    parser.addoption('--token-file', dest='token_file', default=TOKEN)

@pytest.fixture(scope='session')
def vcr(request):
    def auth_matcher(r1, r2):
        return (r1.headers.get('authorization') == \
                r2.headers.get('authorization'))

    def uri_with_query_matcher(r1, r2):
        "URI matcher that allows query params to appear in any order"
        p1,  p2 = urlparse(r1.uri), urlparse(r2.uri)
        return (p1[:3] == p2[:3] and \
                parse_qs(p1.query, True) == parse_qs(p2.query, True))

    # Use `none` to use the recorded requests, and `once` to delete existing
    # cassettes and re-record.
    record_mode = request.config.option.record_mode
    assert record_mode in ('once', 'none')

    cassette_dir = os.path.join(os.path.dirname(__file__), 'cassettes')
    if not os.path.exists(cassette_dir):
        os.makedirs(cassette_dir)

    # https://github.com/kevin1024/vcrpy/pull/196
    vcr = VCR(
        record_mode=request.config.option.record_mode,
        filter_headers=[('Authorization', '**********')],
        filter_post_data_parameters=[('refresh_token', '**********'),
                                     ('code', '**********')],
        match_on=['method', 'uri_with_query', 'auth', 'body'],
        cassette_library_dir=cassette_dir)
    vcr.register_matcher('auth', auth_matcher)
    vcr.register_matcher('uri_with_query', uri_with_query_matcher)
    return vcr

@pytest.yield_fixture()
def reddit(vcr, request):
    cassette_name = '%s.yaml' % request.node.name

    # Clear the cassette before running the test
    recording = (request.config.option.record_mode == 'once')
    if recording:
        filename = os.path.join(vcr.cassette_library_dir, cassette_name)
        if os.path.exists(filename):
            os.remove(filename)

    with vcr.use_cassette(cassette_name):
        logdir = os.path.join(os.path.dirname(__file__), 'log')
        kwargs = { 'token_file': ( \
                                   mkstemp(dir='/var/tmp')[1]
                                   if recording else request.config.option.token_file ),
                   'history_file': mkstemp(dir='/var/tmp')[1],
                   'log_prefix': os.path.join(logdir, 'test_vcr.')
        }
        reddit = AuthenticatedReddit(decode_html_entities=False,
                                     disable_update_check=True,
                                     **kwargs)
        yield reddit
