#lang racket/base
(require racket/system) ; for process/ports
(require "pty.rkt")
(require "fun-terminal.rkt")
;;; what do I need?
;;; cursor needs a location, maybe attrs such as type (I-beam, block...), blink, colors...
;;; sequence handling callbacks
;;;
;;; Questions:  How do programs know when the size changes (eg. vim)?



(provide (all-defined-out)
         (struct-out cell))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; this is to wrap the fun-terminal and hook it together with a process
(define-struct terminal-wrapper
  (fun-terminal
   process-in
   process-out
   redraw-callback
   current-char-handler
   )
  #:mutable)

(define (terminal-wrapper-mutate terminal-wrapper mutator)
  ; takes a function that accepts the fun-terminal
  (set-terminal-wrapper-fun-terminal! terminal-wrapper
                                      (mutator (terminal-wrapper-fun-terminal
                                                terminal-wrapper))))

(define (init-terminal-wrapper command redraw-callback)
  ;; TODO -- the new process needs to start a new process group, do some IOCTL stuff
  (define-values (m-in m-out s-in s-out) (my-openpty))
  (let ((proc (process/ports s-out s-in 'stdout command)))
      (make-terminal-wrapper (make-empty-fun-terminal) m-in m-out redraw-callback null)))

(define (send-char-to-terminal-process term char)
  (write-char char (terminal-wrapper-process-out term))
  (flush-output (terminal-wrapper-process-out term)))

(define (read-char-from-terminal-process term)
  (read-char (terminal-wrapper-process-in term)))

(define (terminal-wrapper-get-lines term)
  (fun-terminal->lines-from-end (terminal-wrapper-fun-terminal term)))

(define (terminal-wrapper-insert-character term char)
  (let ((cell (make-cell char 'foo-color 'bar-color '())))
    (terminal-wrapper-mutate term (lambda (x) (fun-terminal-insert-at-cursor x cell)))))

(define (terminal-wrapper-handle-character term char)
  (define handler (terminal-wrapper-current-char-handler term))
  (cond
    [(not (null? handler)) (handler term char)]
    [((char->integer char) . < . 32)
     (handle-ascii-controls term char)]
    [else
     (terminal-wrapper-insert-character term char)])
  ((terminal-wrapper-redraw-callback term)))

(define (terminal-wrapper-input-listener term)
  (lambda ()
    (define (listener)
      (let ((char (read-char-from-terminal-process term)))
        (if (not (eof-object? char))
            (begin (terminal-wrapper-handle-character term char)
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
    [(#\return #\newline #\u0B #\u0C) (terminal-wrapper-insert-character term char)]
    [(#\u0E) null] ;; activate G1 character set
    [(#\u0F) null] ;; activate G0 character set
    [(#\u1B) null] ;; start escape sequence
    [else null]))
