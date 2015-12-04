Terminal emulator in racket!

Install
-------

You can install from the racket package system by running `raco pkg install rackterm`.  If you want to get it straight from git, clone the repo, `cd` into it, and run `raco pkg install`.

Run
---

`racket -l rackterm/xterm --`
or, if you have your racket/bin directory on your `$PATH`, run `rackterm-xterm`

What is this?
-------------

This is a terminal emulator with support for [24-bit color](https://gist.github.com/XVilka/8346728), italics, and more, written in Racket.

It is a work in progress and is currently lacking many features which I hope to gradually add.  Currently I can run emacs, vim, and other curses programs seemingly just fine.  For an idea of where I want to take the project, see the CONTRIBUTING.md file.

TERM variable
-------------

The rackterm/xterm program runs the tic program on startup to load the rackterm terminfo definition.  It starts with `TERM=rackterm`.  This should work great.  Unless you ssh to another machine, in which case the new host will not have the definition.  You can either scp or otherwise copy the rackterm.terminfo file to the other machine and run `tic rackterm.terminfo` on it, or you can set `TERM=xterm` (perhaps by running with `--term-var xterm`).  You will get fewer capabilities with `TERM=xterm`, like no italics, no extended color, etc, since programs won't know how to access those things, but you don't have to get the terminfo file onto all your machines that way.

OS Support
----------

Rackterm is known to work on GNU/Linux, FreeBSD, and MacOSX.

If it doesn't run on your system, open an issue at https://github.com/willghatch/rackterm and give any information you have.  For Unix, it will be a problem with FFI loading and shouldn't be hard to solve.  For Windows... well, I have no idea how terminals work on Windows, so good luck.

Want to contribute to the future best terminal emulator ever?
-------------------------------------------------------------

See the included `CONTRIBUTING.md` file.  Or, you know, just go make another terminal emulator that will be better than this one, and all others.

License
-------

LGPLv3+.
