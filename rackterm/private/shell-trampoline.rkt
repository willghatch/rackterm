#lang racket/base

;; This is a wrapper to launch another program with proper settings to have job
;; control and let everything work properly in a terminal.  My idea was to have
;; this in racket so no code in other languages would be needed.

(module+ main
  (require "pty.rkt" ffi/unsafe/port)

  (define args (vector->list (current-command-line-arguments)))

  ;; this gives the process a new session ID
  (setsid)

  ;; Some terminal emulators use setpgid too, but it's failing for me
  ;(setpgid 0 0)

  ;; This makes it so emacs won't say "Could not open file: /dev/tty".
  ;; So far it's the only program that runs differently
  (set-controlling-tty (unsafe-port->file-descriptor (current-input-port)))

  (with-handlers ([(lambda (exn) #t)
                   (lambda (exn) ((error-display-handler) (exn-message exn) exn))])
    (apply execvp args)))

