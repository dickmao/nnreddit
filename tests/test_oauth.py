# -*- coding: utf-8 -*-

from __future__ import unicode_literals

try:
    from unittest import mock
except ImportError:
    import mock

def test_oauth(reddit):
    assert reddit._authorized_core is not None
