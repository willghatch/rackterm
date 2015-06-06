#lang racket/base

;; this is simply a wrapper to execute some other program... one of my experiments
;; to properly start the subprocess...

(require "pty.rkt")

(define args (vector->list (current-command-line-arguments)))

(setsid)

(apply rexecvp args)

