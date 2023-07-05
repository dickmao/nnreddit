# -*- coding: utf-8 -*-
# pylint: disable=wrong-import-position

from __future__ import unicode_literals
from __future__ import print_function

import os
import re
import random
import webbrowser
import json
import functools
import sys
import logging
from time import time
import datetime
from multiprocessing import Process
import rtv.config

rtv.config.TEMPLATES = os.path.join(os.path.dirname(__file__), 'templates')
rtv.config.DEFAULT_CONFIG = os.path.join(rtv.config.TEMPLATES, 'rtv.cfg')
rtv.config.DEFAULT_MAILCAP = os.path.join(rtv.config.TEMPLATES, 'mailcap')
rtv.config.HISTORY = os.path.join(rtv.config.XDG_DATA_HOME, 'nnreddit', 'history.log')
rtv.config.TOKEN = os.path.join(rtv.config.XDG_DATA_HOME, 'nnreddit', 'refresh-token')

import praw
from praw import Reddit
from prawcore.sessions import session
from prawcore import Authorizer
from rtv.oauth import OAuthHTTPServer, OAuthHandler
from rtv.exceptions import BrowserError
from rtv import docs

testing = (sys.modules['__main__'].__package__ == 'tests')

if testing:
    from tests.recorded import recording_begin
    from tests.recorded import recording_end
else:
    def recording_begin(*_args):
        pass
    def recording_end(*_args):
        pass
    def recorded(func):
        """Intercept point for Betamax"""
        @functools.wraps(func)
        def wrapper(*args, **kwargs):
            return func(*args, **kwargs)
        return wrapper

with open(os.path.join(os.path.dirname(__file__), 'VERSION'), encoding="utf-8") as version_file:
    version = version_file.read().strip()

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
            stamp = datetime.datetime.fromtimestamp(time()).strftime('%Y%m%d.%H%M%S')
            logging.getLogger().addHandler(logging.FileHandler(log_prefix + stamp))

        localhost = kwargs.pop('localhost', '127.0.0.1')
        default_kwargs = {
            'history_file': rtv.config.HISTORY,
            'token_file': rtv.config.TOKEN,
            'redirect_uri': 'http://' + localhost + ':17973',
            'client_id': '5oagOpX2_NKVDej_iuZjFA',
            'client_secret': '',
            'redirect_port': 17973,
            'user_agent': praw.const.USER_AGENT_FORMAT.\
            format(':'.join([os.uname()[0], 'nnreddit', version])),
        }
        default_kwargs = { k: v for k,v in default_kwargs.items() if k not in kwargs }
        kwargs.update(default_kwargs)
        cfg = rtv.config.Config(**kwargs)
        cfg.load_history()
        cfg.load_refresh_token()
        cfg.config['refresh_token'] = cfg.refresh_token
        logging.getLogger().debug("Refresh token: %s", cfg.token_file)

        super().__init__(**cfg.config)

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
            print("::user::Please check your browser.", file=sys.stderr)
            if cfg.token_file == "/dev/null":
                cfg.refresh_token = None
            else:
                proc = Process(target=self.open_url_silent, args=(url,))
                proc.start()
                try:
                    proc.join(7)
                    if proc.is_alive():
                        raise BrowserError(
                            'Timeout waiting for browser to open')
                finally:
                    try:
                        proc.terminate()
                    except OSError:
                        pass
                server = OAuthHTTPServer(('', cfg.config['redirect_port']), OAuthHandler)
                server.serve_forever()
                self._authorized_core._authorizer.authorize(OAuthHandler.params['code'])
                cfg.refresh_token = self._authorized_core._authorizer.refresh_token
                cfg.save_refresh_token()
        if 'history_size' in cfg.config:
            cfg.save_history()

        self._bodies = {}
        self._stream_comm = {}
        self._stream_subm = {}
        self._stream_inbox = None


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

    # pragma pylint: disable=arguments-differ
    def random_subreddit(self, nsfw=False):
        above = super().random_subreddit(nsfw)
        return above.display_name
    # pragma pylint: enable=arguments-differ

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
        (mytype, myid) = name.split("_", 1)
        parent = None
        if mytype == self.config.kinds['submission']:
            parent = self.submission(myid)
        elif mytype == self.config.kinds['comment']:
            parent = self.comment(myid)
            if q_reply_root and parent.link_id:
                (_, root_id) = parent.link_id.split("_", 1)
                parent = self.submission(root_id)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, mytype))
        parent.reply(body)

    def edit(self, name, body):
        (mytype, myid) = name.split("_", 1)
        editable = None
        if mytype == self.config.kinds['submission']:
            editable = self.submission(myid)
        elif mytype == self.config.kinds['comment']:
            editable = self.comment(myid)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, mytype))
        editable.edit(body)

    def remove(self, name):
        (mytype, myid) = name.split("_", 1)
        editable = None
        if mytype == self.config.kinds['submission']:
            editable = self.submission(myid)
        elif mytype == self.config.kinds['comment']:
            editable = self.comment(myid)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, mytype))
        editable.delete()

    def comments(self, display_name):
        if display_name not in self._stream_comm:
            self._stream_comm[display_name] = self.subreddit(display_name).\
                                           stream.comments(pause_after=0)
        if display_name not in self._bodies:
            self._bodies[display_name] = {}
        dicts = self.collect_dicts(self._stream_comm.get(display_name))
        for dic in dicts:
            if 'body_html' in dic:
                self._bodies[display_name][dic['id']] = dic['body_html']
            else:
                self._bodies[display_name][dic['id']] = 'Wow, such empty'
            for k in list(dic):
                if k.startswith('body'):
                    del dic[k]
        return dicts

    def vote(self, name, vote):
        (mytype, myid) = name.split("_", 1)
        votable = None
        if mytype == self.config.kinds['submission']:
            votable = self.submission(myid)
        elif mytype == self.config.kinds['comment']:
            votable = self.comment(myid)
        else:
            raise ValueError('Unexpected name {} with type {}'.format(name, mytype))
        try:
            if vote == 0:
                votable.clear_vote()
            elif vote < 0:
                votable.downvote()
            else:
                votable.upvote()
        except AttributeError as exc:
            raise AttributeError('{} un-votable: {}'.format(name, str(exc))) from exc

    def body(self, display_name, name):
        (mytype, myid) = name.split("_", 1)

        result = None
        cached = self._bodies.get(display_name)
        if cached:
            result = cached.get(myid)
        if not result:
            if mytype == self.config.kinds['submission']:
                result = self.submission(myid).selftext_html
            else:
                result = self.comment(myid).body_html
        return result

    def canonical_spelling(self, display_name):
        lazy = self.subreddit(display_name)
        # pragma pylint: disable=protected-access
        lazy._fetch()
        # pragma pylint: enable=protected-access
        return lazy.display_name

    def submissions(self, display_name):
        if display_name not in self._stream_subm:
            self._stream_subm[display_name] = self.subreddit(display_name).\
                                              stream.submissions(pause_after=0)
        if display_name not in self._bodies:
            self._bodies[display_name] = {}
        dicts = self.collect_dicts(self._stream_subm.get(display_name))
        for dic in dicts:
            if 'selftext_html' in dic:
                self._bodies[display_name][dic['id']] = dic['selftext_html']
            elif 'url' in dic:
                self._bodies[display_name][dic['id']] \
                    = ''.join(['<div>', '<p>',
                               '<a href="{0}">{0}</a>'.format(dic.get('url')),
                               '</div>'])
            else:
                self._bodies[display_name][dic['id']] = 'Wow, such empty'
            for k in list(dic):
                if k.startswith('selftext'):
                    del dic[k]
        return dicts

    def user_subreddits(self):
        return [sr.display_name for sr in self.user.subreddits()]

    def user_attr(self, attr):
        return getattr(self.user.me(), attr)

    def inboxes(self, inbox_name):
        if not self._stream_inbox:
            self._stream_inbox = self.inbox.stream(pause_after=0)
        if inbox_name not in self._bodies:
            self._bodies[inbox_name] = {}
        dicts = self.collect_dicts(self._stream_inbox)
        dicts = [d for d in dicts if d.get('type') == 'comment_reply' or
                 d.get('type') == 'post_reply']
        for dic in dicts:
            if 'body_html' in dic:
                self._bodies[inbox_name][dic['id']] = dic['body_html']
            else:
                self._bodies[inbox_name][dic['id']] = 'Wow, such empty'
            for k in list(dic):
                if k.startswith('body'):
                    del dic[k]
        return dicts
