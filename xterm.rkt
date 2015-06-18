#lang racket/base

;; This is to be executed, running a GUI terminal emulator

(require racket/class)
(require racket/gui/base)
(require racket/cmdline)
(require "terminal.rkt")
(require "terminal-canvas.rkt")
(require "key-tree.rkt")

;; to run tic
(require racket/system)
(require racket/runtime-path)


(define font-size (make-parameter 12))
(define font-name (make-parameter "use first fallback"))
(define command (make-parameter #f))
(define term-var (make-parameter "rackterm"))

(define command-args
  (command-line
   #:once-each
   [("--font-name") fontname
    "Use the given font."
    (font-name fontname)]
   [("--font-size") size
    "Use the given font size."
    (font-size (string->number size))]
   [("--term-var") TERM
    "override default TERM value of 'rackterm'."
    (term-var TERM)]
   [("-e" "--command") cmd
    "Execute the given command as the shell."
    (command cmd)]
   #:args args
   args))

(define command-and-args (if (command)
                             (append (list (command)) command-args)
                             (list (or (getenv "SHELL")
                                       "/bin/sh")
                                   "-i")))

(define xterm-frame%
  (class frame%
    (init-field
     [handling-key-tree
      (keyhandler #f
                  (key 'control #\G) (lambda () (send this add-canvas))
                  (key 'control #\N) (lambda () (send this focus-next))
                  )])

    (define current-key-tree handling-key-tree)
    (define/public (set-current-handler-tree ktree)
      (set! current-key-tree ktree))
    (define/override (on-subwindow-char receiver event)
      (define key-ev (map-char-event-to-key-tree-event event))
      (define handler (key-tree-get current-key-tree key-ev))
      (cond [(key-tree? handler) (begin
                                   (set! current-key-tree handler)
                                   #t)]
            ;; if the handler returns 'pass-through, let control pass through to the child
            [handler (begin
                       (set! current-key-tree handling-key-tree)
                       (define ret ((at-least-one-aritize handler) key-ev))
                       (if (equal? ret 'pass-through) #f #t))]
            ;; if handler is #f but the handler-tree was not the default, eat the key (don't pass it)
            [(not (equal? current-key-tree handling-key-tree)) #t]
            ;; otherwise just let the kids handle it.
            [else #f]))

    (define/public (add-canvas)
      (let ((c (new terminal-canvas%
                    [parent this]
                    [font-size (font-size)]
                    [font-name (font-name)]
                    [command-and-args command-and-args]
                    [set-title-callback (lambda (title) (send this set-label title))]
                    [horiz-margin 2]
                    [vert-margin 2]
                    )))
        (send c focus)
        c))
    (define/public (focus-next)
      (let* ((children (send this get-children))
             (focused-child (memf (lambda (c) (send c has-focus?)) children))
             (next-child (if focused-child (cdr focused-child) #f)) 
             (to-focus (if (or (null? next-child) (not next-child))
                           (car children)
                           (car next-child))))
        (send to-focus focus)))

    (super-new)))


(module+ main
  ;; Let's just run tic here and not have others worry about this terminfo crap.
  (define-runtime-path terminfo-file "rackterm.terminfo")
  (system (string-append "tic " (path->string terminfo-file)))

  (define frame (new xterm-frame%
                     [label "racket xterm"]
                     [width 800]
                     [height 800]
                     ))

  (send frame show #t)
  (send frame add-canvas))


