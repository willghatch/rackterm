#lang racket/base

;; This is a wrapper to launch another program with proper settings to have job
;; control and let everything work properly in a terminal.  My idea was to have
;; this in racket so no code in other languages would be needed.

(require "pty.rkt")

(define args (vector->list (current-command-line-arguments)))

;; this gives the process a new session ID
(setsid)

;; Some terminal emulators use setpgid too, but it's failing for me
;(setpgid 0 0)

;; This makes it so emacs won't say "Could not open file: /dev/tty".
;; So far it's the only program that runs differently
(set-controlling-tty (scheme_get_port_file_descriptor (current-input-port)))

(apply execvp args)

