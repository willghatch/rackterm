Terminal emulator in racket!

Very much a work in progress, started as a project to learn Racket, though I hope it grows beyond that to be one I can use as my daily driver.  Therefore there are various design goals for... some day.

It is not yet really packaged or anything.  To run the xterm, run `racket xterm.rkt`.

It has [24-bit color support](https://gist.github.com/XVilka/8346728), as well as 256color support and standard 8 color support.  Other styling is, at the time of writing, not supported (bold, underline, blink, etc).

At this point I seem to be able to run vim and emacs on it fine.

Right now it depends on the setsid(1) command to spawn child processes that can have job control, and depends on some hard-coded constants that are likely only valid for Linux, so it's not (presently) portable.
