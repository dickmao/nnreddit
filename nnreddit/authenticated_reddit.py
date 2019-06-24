# -*- coding: utf-8 -*-
# pylint: disable=wrong-import-position

from __future__ import unicode_literals
from __future__ import print_function

import rtv.config
import os

rtv.config.TEMPLATES = os.path.join(os.path.dirname(__file__), 'templates')
rtv.config.DEFAULT_CONFIG = os.path.join(rtv.config.TEMPLATES, 'rtv.cfg')
rtv.config.DEFAULT_MAILCAP = os.path.join(rtv.config.TEMPLATES, 'mailcap')
rtv.config.HISTORY = os.path.join(rtv.config.XDG_DATA_HOME, 'nnreddit', 'history.log')
rtv.config.TOKEN = os.path.join(rtv.config.XDG_DATA_HOME, 'nnreddit', 'refresh-token')

import praw
import re
import random
import webbrowser
import json
import functools
import sys
import logging
from time import time
import datetime

from praw import Reddit
from prawcore.sessions import session
from prawcore import Authorizer
from rtv.oauth import OAuthHTTPServer, OAuthHandler
from multiprocessing import Process
from rtv.exceptions import BrowserError
from rtv import docs

testing = (sys.modules['__main__'].__package__ == 'tests')

if testing:
    from tests.recorded import recording_begin
    from tests.recorded import recording_end
else:
    def recording_begin(*args):
        pass
    def recording_end(*args):
        pass
    def recorded(func):
        """Intercept point for Betamax"""
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)
        return wrapper

__version__ = '0.1.0'

class AuthenticatedReddit(Reddit):
    @staticmethod
    def open_url_silent(url):
        stdout, stderr = os.dup(1), os.dup(2)
        null = os.open(os.devnull, os.O_RDWR)
        try:
            os.dup2(null, 1)
            os.dup2(null, 2)
            webbrowser.open_new_tab(url)
        finally:
            try:
                os.close(null)
            except OSError:
                pass
            os.dup2(stdout, 1)
            os.dup2(stderr, 2)

    def __init__(self, **kwargs):
        log_level = kwargs.pop('log_level', logging.NOTSET)
        logging.basicConfig(level=log_level,
                            filename='/dev/null',
                            format='%(asctime)s %(levelname)s %(message)s',
                            datefmt="%Y-%m-%d %H:%M:%S")
        log_prefix = kwargs.pop('log_prefix', None)
        if log_prefix:
            ts = datetime.datetime.fromtimestamp(time()).strftime('%Y%m%d.%H%M%S')
            logging.getLogger().addHandler(logging.FileHandler(log_prefix + ts))

        default_kwargs = {
            'history_file': rtv.config.HISTORY,
            'token_file': rtv.config.TOKEN,
            'redirect_uri': 'http://127.0.0.1:17973',
            'client_id': 'KBV2seGZgHOa9g',
            'client_secret': 'cannot-be-empty',
            'redirect_port': 17973,
            'user_agent': praw.const.USER_AGENT_FORMAT.\
            format(':'.join([os.uname()[0], 'nnreddit', __version__])),
        }
        default_kwargs = { k: v for k,v in default_kwargs.items() if k not in kwargs }
        kwargs.update(default_kwargs)
        cfg = rtv.config.Config(**kwargs)
        cfg.load_history()
        cfg.load_refresh_token()
        cfg.config['refresh_token'] = cfg.refresh_token
        logging.getLogger().debug("Refresh token: %s", cfg.token_file)

        super(AuthenticatedReddit, self).__init__(**cfg.config)

        if not cfg.refresh_token:
            self._core \
                = self._authorized_core \
                = session(Authorizer(self._core._authorizer._authenticator))
            state = str(random.randint(0, 65000))
            url = self._authorized_core._authorizer._authenticator.\
                  authorize_url('permanent', ['edit',
                                              'history',
                                              'identity',
                                              'mysubreddits',
                                              'privatemessages',
                                              'read',
                                              'report',
                                              'save',
                                              'submit',
                                              'subscribe',
                                              'vote'], state)

            docs_sub = re.compile(r'reddit terminal viewer', re.IGNORECASE)
            docs.OAUTH_SUCCESS = docs_sub.sub('nnreddit', docs.OAUTH_SUCCESS)
            docs.OAUTH_ACCESS_DENIED = docs_sub.sub('nnreddit', docs.OAUTH_ACCESS_DENIED)
            server = OAuthHTTPServer(('', cfg.config['redirect_port']), OAuthHandler)
            p = Process(target=self.open_url_silent, args=(url,))
            p.start()
            try:
                p.join(7)
                if p.is_alive():
                    raise BrowserError(
                        'Timeout waiting for browser to open')
            finally:
                try:
                    p.terminate()
                except OSError:
                    pass
            server.serve_forever()
            self._authorized_core._authorizer.authorize(OAuthHandler.params['code'])
            cfg.refresh_token = self._authorized_core._authorizer.refresh_token
            cfg.save_refresh_token()
        if 'history_size' in cfg.config:
            cfg.save_history()

        self._bodies = {}
        self._stream_comm = {}
        self._stream_subm = {}

    @staticmethod
    def make_dict(reddit_base):
        with_nulls = json.dumps(vars(reddit_base), skipkeys=True, default=lambda o:
                                str(o) if hasattr(o, 'STR_FIELD') else None)
        without_nulls = {k:v for k,v in json.loads(with_nulls).items()
                         if v is not None}
        return without_nulls

    @staticmethod
    def collect_dicts(stream):
        result = []
        for i in stream:
            if i is None:
                break
            result.append(AuthenticatedReddit.make_dict(i))
        return result

    def recording_begin(self, cassette):
        recording_begin(self, cassette)
        return True

    def recording_end(self, cassette=None):
        recording_end(cassette)
        return True

    def random_subreddit(self, nsfw=False):
        sr = super(AuthenticatedReddit, self).random_subreddit(nsfw)
        return sr.display_name

    def search(self, query, **generator_kwargs):
        return [ x.display_name for x in self.subreddits.search(query, **generator_kwargs) ]

    def popular(self, **generator_kwargs):
        return [ x.display_name for x in self.subreddits.popular(**generator_kwargs) ]

    def subscribe(self, display_name):
        self.subreddit(display_name).subscribe()

    def unsubscribe(self, display_name):
        self.subreddit(display_name).unsubscribe()

    def submit(self, display_name, title, **kwargs):
        self.subreddit(display_name).submit(title, **kwargs)

    def reply(self, name, body, q_reply_root):
        (type, id) = name.split("_", 1)
        parent = None
        if type == self.config.kinds['submission']:
            parent = self.submission(id)
        elif type == self.config.kinds['comment']:
            parent = self.comment(id)
            if q_reply_root and parent.link_id:
                (_, root_id) = parent.link_id.split("_", 1)
                parent = self.submission(root_id)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, type))
        parent.reply(body)

    def edit(self, name, body):
        (type, id) = name.split("_", 1)
        editable = None
        if type == self.config.kinds['submission']:
            editable = self.submission(id)
        elif type == self.config.kinds['comment']:
            editable = self.comment(id)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, type))
        editable.edit(body)

    def delete(self, name):
        (type, id) = name.split("_", 1)
        editable = None
        if type == self.config.kinds['submission']:
            editable = self.submission(id)
        elif type == self.config.kinds['comment']:
            editable = self.comment(id)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, type))
        editable.delete()

    def comments(self, display_name):
        if display_name not in self._stream_comm:
            self._stream_comm[display_name] = self.subreddit(display_name).\
                                           stream.comments(pause_after=0)
        if display_name not in self._bodies:
            self._bodies[display_name] = {}
        dicts = self.collect_dicts(self._stream_comm.get(display_name))
        for d in dicts:
            if 'body_html' in d:
                self._bodies[display_name][d['id']] = d.get('body_html')
            else:
                self._bodies[display_name][d['id']] = 'Wow, such empty'
            for k in list(d):
                if k.startswith('body'):
                    del d[k]
        return dicts

    def vote(self, name, vote):
        (type, id) = name.split("_", 1)
        votable = None
        if type == self.config.kinds['submission']:
            votable = self.submission(id)
        elif type == self.config.kinds['comment']:
            votable = self.comment(id)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, type))
        try:
            if vote == 0:
                votable.clear_vote()
            elif vote < 0:
                votable.downvote()
            else:
                votable.upvote()
        except AttributeError as e:
            raise AttributeError('{} un-votable: {}'.format(name, str(e)))
        return

    def body(self, display_name, name):
        (type, id) = name.split("_", 1)

        result = None
        cached = self._bodies.get(display_name)
        if cached:
            result = cached.get(id)
        if not result:
            if type == self.config.kinds['submission']:
                result = self.submission(id).selftext_html
            else:
                result = self.comment(id).body_html
        return result

    def submissions(self, display_name):
        if display_name not in self._stream_subm:
            self._stream_subm[display_name] = self.subreddit(display_name).\
                                              stream.submissions(pause_after=0)
        if display_name not in self._bodies:
            self._bodies[display_name] = {}
        dicts = self.collect_dicts(self._stream_subm.get(display_name))
        for d in dicts:
            if 'selftext_html' in d:
                self._bodies[display_name][d['id']] = d.get('selftext_html')
            elif 'url' in d:
                self._bodies[display_name][d['id']] \
                    = ''.join(['<div>', '<p>',
                               '<a href="{0}">{0}</a>'.format(d.get('url')),
                               '</div>'])
            else:
                self._bodies[display_name][d['id']] = 'Wow, such empty'
            for k in list(d):
                if k.startswith('selftext'):
                    del d[k]
        return dicts

    def user_subreddits(self):
        return [sr.display_name for sr in self.user.subreddits()]

    def user_attr(self, attr):
        return getattr(self.user.me(), attr)
