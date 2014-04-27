Title: The System Pip On Lion Is Completely Broken??
Slug: the-system-pip-on-lion-is-completely-broken
Tags:
    python
    bugs
Posted: 2012-02-18T19:58:00Z
I sat down to hack on [Pullme](https://github.com/andrewlorente/pullme) for the first time on my new macbook. I ran into unreasonable roadblocks:

```
$ mkvirtualenv pullme
Running virtualenv with interpreter /usr/local/Cellar/python/2.7.2/bin/python
Overwriting pullme/lib/python2.7/orig-prefix.txt with new content
New python executable in pullme/bin/python
Installing setuptools.............done.
Installing pip...............done.
Traceback (most recent call last):
  File "<string>", line 1, in <module>
ImportError: No module named virtualenvwrapper.hook_loader
```

Augh, what is happening? Well, for some reason the pip that comes with OSX Lion puts things in a directory that's _totally unrelated_ to the directories that the system Python searches:

```
$ python -c 'import sys; print "\n".join(map(repr, sys.path))'
''
'/usr/local/Cellar/python/2.7.2/lib/python2.7/site-packages/distribute-0.6.24-py2.7.egg'
'/usr/local/Cellar/python/2.7.2/lib/python2.7/site-packages/pip-1.1-py2.7.egg'
...snip...
'/usr/local/Cellar/python/2.7.2/lib/python2.7/lib-dynload'
'/usr/local/Cellar/python/2.7.2/lib/python2.7/site-packages'
'/usr/local/Cellar/python/2.7.2/lib/python2.7/site-packages/setuptools-0.6c11-py2.7.egg-info'
```

Python is only looking in `/usr/local/Cellar/...`

```
$ locate virtualenvwrapper
/Library/Python/2.7/site-packages/virtualenvwrapper
/Library/Python/2.7/site-packages/virtualenvwrapper/hook_loader.py
/Library/Python/2.7/site-packages/virtualenvwrapper/hook_loader.pyc
...snip...
/Library/Python/2.7/site-packages/virtualenvwrapper-3.0-py2.7.egg-info/requires.txt
/Library/Python/2.7/site-packages/virtualenvwrapper-3.0-py2.7.egg-info/top_level.txt
/usr/local/bin/virtualenvwrapper.sh
```

Meanwhile, `virtualenvwrapper` is installed in `/Library/Python/...`

I solved the problem with a new pip:

```
$ easy_install pip
Searching for pip
Reading http://pypi.python.org/simple/pip/
Reading http://www.pip-installer.org
...snip...
Installed /usr/local/lib/python2.7/site-packages/pip-1.1-py2.7.egg
Processing dependencies for pip
Finished processing dependencies for pip

$ type pip
pip is hashed (/usr/local/bin/pip)

$ hash -r

$ type pip
pip is /usr/local/share/python/pip

$ pip install virtualenvwrapper
Downloading/unpacking virtualenvwrapper
  Downloading virtualenvwrapper-3.0.tar.gz (717Kb): 717Kb downloaded
  Running setup.py egg_info for package virtualenvwrapper
...snip...
    Installing virtualenv script to /usr/local/share/python
Successfully installed virtualenvwrapper virtualenv
Cleaning up...

$ mkvirtualenv pullme
New python executable in pullme/bin/python
Installing setuptools.............done.
Installing pip...............done.
virtualenvwrapper.user_scripts creating /Users/andrewlorente/Envs/pullme/bin/predeactivate
virtualenvwrapper.user_scripts creating /Users/andrewlorente/Envs/pullme/bin/postdeactivate
virtualenvwrapper.user_scripts creating /Users/andrewlorente/Envs/pullme/bin/preactivate
virtualenvwrapper.user_scripts creating /Users/andrewlorente/Envs/pullme/bin/postactivate
virtualenvwrapper.user_scripts creating /Users/andrewlorente/Envs/pullme/bin/get_env_details
```

Success!

It's possible I actually installed `pip` some crazy way, but I don't know where I would've gotten it other than `easy_install`. If Lion really is shipping with a `pip` that _doesn't install packages to Python's `sys.path`_, well geez, that's just terrible.
