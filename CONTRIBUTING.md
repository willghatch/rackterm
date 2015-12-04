
I have big plans for this terminal emulator, but probably will not have a lot of time to work on it.  I want to lay out my ideas here so that I'll remember them and so that potential contributors can understand where I intend the project to go.  Feedback on these plans are welcome and encouraged - I would like to know other people's thoughts.  I don't want to make a terminal emulator to just do what is already done, and I don't do everything in the terminal because I'm old-fashioned or some sort of luddite.  I use terminals because it can be a much more powerful and efficient way of interacting with a computer than pointing around with the rat and clicking on the few options that can be visually displayed.  I want to move away from a lot of the historical baggage of terminal emulators and their use that have grown since the 60s or 70s.  So feel free to contribute ideas or code if you are a like-minded terminal lover.

By the way, if you contribute code, it's currently somewhat a mess -- the rationale for actually starting this was to learn racket, and a bunch of the code is really gross as I threw it together as fast as I could while learning how to make things work.

Plans
=====

* I want to use the core library to make multiple frontends -- I want to have it back an xterm, a GUI terminal widget (canvas%, so far, that is used in the xterm, but can also be embedded in other gui programs if desired), a tmux/screen style console terminal emulator/multiplexer, and any other frontend (I imagine a weekend project of a framebuffer terminal, some day, or at some point being able to run racket on Android and replacing the no-longer-developed-or-accepting-new-features terminal for Android...).  My plan is for the terminal object to keep track of all the state and the frontend will just draw it out and send input to it.
* I want to extend the capabilities of terminal emulators past what is currently done -- I want to add graphical elements in the forms of:
    - GUI cells (IE bitmaps can be drawn in cells that would normally hold characters)
    - overlays (IE drawn on an otherwise transparent layer above the characters.
* This would allow for things such as:
    - drracket style drawing lines to show usages of a symbol
    - emacs, powerline, etc to draw weird pictures/glyphs without requiring special modified fonts
    - some people want to add gui symbols, controls, etc to console programs (I'm less on board here, because I hate anything that will end up requiring mouse interaction, but any well written program should be able to avoid that).
    - various other things I haven't even thought of, I'm sure
* I want to get away from parsing crazy escape codes, and I want to get away from terminal capabilities being inferred by some third party database.  The days are long gone when people connected a dumb hardware terminal to a serial port.  Now every terminal is a smart terminal, so the terminal should be able to talk with the client program and simply tell it its capabilities.  To do these, I want to add an embedded interpreter.  There are codes that already accept a field of arbitrary text -- ESC ] <number> ; <text> <string-terminator>.  I want to add one that will be for evaluating... probably scheme, with added "terminal primitives" for all needed terminal operations.  I figure R5RS or some such will be adoptable by other terminal emulators if the idea ever became popular, so code written to run on a rackterm can also work on other terminals that decide to implement it, via one of the myriad of embeddable schemes.  This will have to be combined with a canonical format of sending any necessary responses back to the client -- it can just expect to receive a certain similar escape sequence soon after sending a request that needs a reply.  The rationale behind having replies and such is so that a client can ask about capabilities of the interpreter (IE are you r5rs, r6rs...?  Do you support italics?  Do you support slanting the opposite way of italics? etc).
* I want it to be very configurable (probably in racket code itself) - both the tmux/screen version and a future multiplexing xterm version will need various commands to split the view and open another terminal, switch over to another background window of terminals, do window management tasks, etc.  I want the key configuration to have first class support for modality (a la vim -- if it's not there someone always wants to hack it in anyhow, eg. [tmuxmodality.py](https://github.com/mtl/tmux-modality)), and chains of keys (eg C-c C-n, like emacs has for so much stuff).  The default config need not use either of those, but I'm quite sure I will use both.
* I want there to be good configuration for mapping input characters to other things -- arrow keys, other named keys, function keys, etc need these weird mappings.  You can also add mappings so you can use super and hyper modifiers in the terminal (like control and alt).  I would also like to be able to map C-m and C-i, which are the same as return and tab, respectively.  Then there is always the debate over whether the backspace key should send BS or DEL...
* I would like the bulk of configuration to be able to be shared by the various frontends to rackterm, but individual modifications to be made to each.  Also, the configuration should be able to source more than one file.  A common pattern I use is to keep my main configuration for programs in my dotfiles, but source a local configuration file if it exists to add any extra configuration I need on a specific computer.  Common differences may include font sizes, for example.
* Since I want configuration to be in full racket, but many people like simple, weak configuration, perhaps it could load configuration in multiple formats, or there could be a #lang rackterm-config which would make it easy.  The configuration could be loaded with some dynamic-require or perhaps xmonad-style by wrapping the original code with the configuration to make a new executable.
* And, of course, I want all the expected terminal features that I don't have yet -- scrollback, mouse support, copy/paste with display server, etc.
* And I'd like to have some sort of mode to, say, rather than spawning the child process itself and handling things the Unix way, it might be good to have it be able to connect to an actual serial input, or be an ssh terminal (eg. in place of putty on Windows), so I should add support for that at some point.
* I'd like it to not be super slow like it is now... though as long as it's "good enough" efficiency will probably be a secondary concern for me.

Code Layout
===========

For the moment, at least.  This could be outdated later.

At the center, the terminal emulator stores its state in a functional zipper, in `fun-terminal.rkt`.
`cell.rkt` has the data structures for the character cells that the zipper holds.
I parse console codes into s-expressions in `console-code-parse.rkt`
`pty.rkt` has a bit of code for interfacing with the Unix PTY system.
`shell-trampoline.rkt` is a little wrapper for spawned programs to be able to set some Unix stuff before actually executing the provided shell.
`terminal.rkt` is in charge of communicating with the sub-process, interpreting the s-expressions to mutate the state, etc.  Basically it is everything but some sort of (potentially graphical) visualization and UI.
`terminal-canvas.rkt` provides a canvas% class that can be embedded in any Racket gui.
`xterm.rkt` is a full-blown application that basically just wraps the canvas.

I hope to improve the interfaces of these things to make it useful for any program to
embed a terminal in its GUI if it wants, or parse ANSI console codes for some non-terminal
use in some application, etc.


