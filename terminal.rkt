#lang racket/base
(require racket/system) ; for process/ports
(require "pty.rkt")
(require "fun-terminal.rkt")


(provide (all-defined-out)
         (struct-out cell))


;; TODO:
;; I want something better than wrapping a command in setsid to set a new process group
;; the 'who' command doesn't show my racket xterms...


(define-struct terminal
  (fun-terminal
   process-in
   process-out
   redraw-callback
   current-char-handler
   )
  #:mutable)

(define (terminal-mutate terminal fun-terminal-function)
  (set-terminal-fun-terminal! terminal
                              (fun-terminal-function (terminal-fun-terminal
                                                      terminal))))

(define (terminal-insert-at-cursor term cell)
  (terminal-mutate term (lambda (ft) (fun-terminal-insert-at-cursor ft cell))))
(define (terminal-delete-backwards-at-cursor term)
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-backwards-at-cursor ft))))
(define (terminal-line-break-at-cursor term)
  (terminal-mutate term (lambda (ft) (fun-terminal-line-break-at-cursor ft))))

(define (init-terminal command redraw-callback)
  (define-values (m-in m-out s-in s-out) (my-openpty))
  (let ((proc (process/ports s-out s-in 'stdout command)))
      (make-terminal (make-empty-fun-terminal) m-in m-out redraw-callback null)))

(define (send-char-to-terminal-process term char)
  (write-char char (terminal-process-out term))
  (flush-output (terminal-process-out term)))

(define (terminal-get-lines term)
  (fun-terminal->lines-from-end (terminal-fun-terminal term)))

(define (terminal-insert-character term char)
  (let ((cell (make-cell char 'foo-color 'bar-color '())))
    (terminal-insert-at-cursor term cell)))

(define (terminal-handle-character term char)
  (define handler (terminal-current-char-handler term))
  (cond
    [(not (null? handler)) (handler term char)]
    [((char->integer char) . < . 32)
     (handle-ascii-controls term char)]
    [else
     (terminal-insert-character term char)])
  ((terminal-redraw-callback term)))

(define (terminal-input-listener term)
  (define (read-char-from-terminal-process term)
    (read-char (terminal-process-in term)))
  (lambda ()
    (define (listener)
      (let ((char (read-char-from-terminal-process term)))
        (if (not (eof-object? char))
            (begin (terminal-handle-character term char)
                   (listener))
            null)))
    (sleep 0)
    (listener)))

(define (handle-ascii-controls term char)
  (case char
    [(#\u07) null] ;; BEEP!
    [(#\u08) null] ;; backspace
    [(#\u09) null] ;; tab
    ;; technically return should give a carriage return, all others a line feed...
    [(#\return #\newline #\u0B #\u0C) (terminal-insert-character term char)]
    [(#\u0E) null] ;; activate G1 character set
    [(#\u0F) null] ;; activate G0 character set
    [(#\u1B) null] ;; start escape sequence
    [else null]))
