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

The rackterm/xterm program runs the tic program on startup to load the rackterm terminfo definition.  It starts with `TERM=rackterm`

Fonts
-----

They'll be configurable soon, but FYI if your platform doesn't have the default font "Deja Vu Sans Mono" installed, it will dis-gracefully degrade to a really bad font and look ridiculous.

OS Support
----------

Rackterm is known to work on GNU/Linux and MacOSX.  If installing a FreeBSD virtual machine and running anything on it weren't such a hassle, I would have tested it, but it should work as well.

If it doesn't run on your system, open an issue at https://github.com/willghatch/rackterm and give any information you have.  For Unix, it will be a problem with FFI loading and shouldn't be hard to solve.  For Windows... well, I have no idea how terminals work on Windows, so good luck.

Want to contribute to the future best terminal emulator ever?
-------------------------------------------------------------

See the included `CONTRIBUTING.md` file.  Or, you know, just go make another terminal emulator that will be better than this one, and all others.

License
-------

Same as racket itself -- LGPLv3.
