Terminal emulator in racket!

Install
-------

Clone the repo, `cd` into it, and run `raco pkg install`.

Run
---

`racket -l rackterm/xterm`

What is this?
-------------

This is a terminal emulator with support for [24-bit color](https://gist.github.com/XVilka/8346728), italics, and more, written in Racket.

It is a work in progress and is currently lacking many features which I hope to gradually add.  Currently I can run emacs, vim, and other curses programs seemingly just fine.  For an idea of where I want to take the project, see the CONTRIBUTING.md file.

OS Support
----------

So far I know it runs on Arch Linux.  It depends on some FFI code, and has some hard coded values of some C constants which may differ between systems.  They should stay pretty static on any given system, since people don't want to break existing executables, but they may differ between platforms.  If it doesn't work on your platform, raise an issue in the issue tracker at https://github.com/willghatch/rackterm please.

All of my personal computers run GNU/Linux, and I don't really have access to a Mac, and I have no idea if this could, say, run on cygwin.  I'm quite sure it can run on any BSD, as long as I get the constants and the shared libraries to load right, but I don't really want to go set up a VM for *BSD unless I find out that someone actually wants to use this on that platform.  And if you're going to use it on one of these platforms, you might as well help me figure out the constant situation.

To that end, I've included `constants.c`, which you should be able to compile and run to tell me what the constants are.  For convenience, `constants.sh` does just that.  If there are errors in compilation, it means that those constants are defined somewhere different than they are on GNU/Linux systems.  Dang.  I hope you can figure out where they are!

Also, I will need to load the correct shared library.  Here is what is used:

In `libutil` I have `ioctl` and `openpty`.  This one could be problematic.

In `libc` I have `setsid`, `setpgid`, and `execvp`.  I don't expect there to be a problem with this one.

I would love for this to be cross platform, so if anybody cares, please send in anything you find about that.

Want to contribute to the future best terminal emulator ever?
-------------------------------------------------------------

See the included `CONTRIBUTING.md` file.  Or, you know, just go make another terminal emulator that will be better than this one, and all others.

License
-------

Same as racket itself -- LGPLv3.
