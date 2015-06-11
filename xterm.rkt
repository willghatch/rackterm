#lang racket/base

;; This is to be executed, running a GUI terminal emulator

(require racket/class)
(require racket/gui/base)
(require racket/cmdline)
(require "terminal.rkt")
(require "terminal-canvas.rkt")


(define frame (new frame%
                   [label "racket xterm"]
                   [width 800]
                   [height 800]))

(send frame show #t)

(define font-size (make-parameter 12))
(define font-name (make-parameter "DejaVu Sans Mono"))
(define command (make-parameter #f))

(define command-args
  (command-line
   #:final
   [("-e" "--command") cmd
    "Execute the given command as the shell."
    (command cmd)]
   #:once-each
   [("--font-name") fnt
    "Use the given font."
    (font-name fnt)]
   [("--font-size") size
    "Use the given font size."
    (font-size (string->number size))]
   #:args args
   args))

(define command-and-args (if (command)
                             (append (list (command)) command-args)
                             (list (or (getenv "SHELL")
                                       "/bin/sh")
                                   "-i")))

(define the-canvas
  (new terminal-canvas%
       [parent frame]
       [font-size (font-size)]
       ;; How can I tell if a font name exists on a system?  If I give a bogus
       ;; font name, it falls back to some lame non-monospaced font that looks
       ;; terrible...
       [font-name (font-name)]
       [command-and-args command-and-args]
       ))

;(define b-canvas
;  (new terminal-canvas%
;       [parent frame]
;       [font "Terminal"]
;       ))

;; TODO -- use on-subwindow-char/on-subwindow-event to steal events to do stuff,
;; eg. split window, do tab stuff, etc.

;; Let's just run tic here and not have others worry about this terminfo crap.
(require racket/system)
(require racket/runtime-path)
(define-runtime-path terminfo-file "rackterm.terminfo")
(system (string-append "tic " (path->string terminfo-file)))


(send the-canvas focus)

