#lang racket/base

(require "pty.rkt")

;; current-command-line-args should be '(tty-fd command args)
(define args (vector->list (current-command-line-arguments)))
(define tty-fd (string->number (car args)))
(define command (cadr args))
(define command-args (cddr args))
(define applied (append (list tty-fd command) command-args))

(setsid)

(apply exec-with-new-tty (append
                          (list
                           (scheme_get_port_file_descriptor (current-output-port))
                           command)
                          command-args))

