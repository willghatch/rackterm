#lang racket/base
(require racket/system) ; for process/ports
(require "pty.rkt")
;;; what do I need?
;;; cursor needs a location, maybe attrs such as type (I-beam, block...), blink, colors...
;;; sequence handling callbacks
;;;
;;; Questions:  How do programs know when the size changes (eg. vim)?

;; Cells - each cell has RGBA color, background, a unicode character, attributes such as underline, blink, italic... similar to emacs face properties.  Maybe font?  Cell size?  Images could be large cells?
(define-struct cell
  (character fg-color bg-color attr-list)) ; maybe just have a list of attrs, so
  ; the average cell doesn't have as much data...

;; lines are a series of cells.  I should have normal mode output keep in one line and wrap it according to terminal size (and re-wrapping on size change)


(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct fun-terminal
  (lines-before-cursor ; reversed from normal order
   lines-after-cursor
   line-with-cursor
   length-lines-before-cursor
   length-lines-after-cursor))

(define-struct cursor-line
  (cells-before-cursor ; reversed list
   cells-after-cursor ; non-reversed list
   length-cells-before-cursor)) ; AKA cursor position
(define (make-empty-cursor-line)
  (make-cursor-line '() '() 0))

(define (make-empty-fun-terminal)
  (make-fun-terminal '() '() (make-empty-cursor-line) 0 0))

(define (cursor-line->normal-line line)
  (foldl cons (cursor-line-cells-before-cursor line) (cursor-line-cells-after-cursor line)))

(define (normal-line->cursor-line line [line-index 0])
  (let ((end (list-tail line line-index))
        (beg (reverse (for/list ([elem line]
                                 [i (in-naturals)]
                                 #:break (i . >= . line-index))
                        elem))))
    (make-cursor-line beg end line-index (length end))))

(define (cursor-line-empty-after-cursor? line)
  (not (null? (cursor-line-cells-after-cursor line))))
(define (cursor-line-empty-before-cursor? line)
  (not (null? (cursor-line-cells-before-cursor line))))

(define (cursor-line-delete-cell-forward line)
  (struct-copy cursor-line line
               [cells-after-cursor (cdr (cursor-line-cells-after-cursor line))]))
(define (cursor-line-delete-cell-backward line)
  (struct-copy cursor-line line
               [cells-before-cursor (cdr (cursor-line-cells-before-cursor line))]
               [length-cells-before-cursor (- (cursor-line-length-cells-before-cursor
                                               line) 1)]))
(define (cursor-line-insert-cell line cell)
  (struct-copy cursor-line line
               [cells-before-cursor (cons cell (cursor-line-cells-before-cursor line))]
               [length-cells-before-cursor (+ 1 (cursor-line-length-cells-before-cursor line))]))

(define (move-cursor-line terminal [direction 'forward] [line-index 'current])
  ;; TODO - check that I'm not moving past the end/beginning
  (let* ((old-cursor-line (fun-terminal-line-with-cursor terminal))
         (old-cursor-line-normalized (cursor-line->normal-line old-cursor-line))
         (old-before (fun-terminal-lines-before-cursor terminal))
         (old-after (fun-terminal-lines-after-cursor terminal))
         (index (if (equal? line-index 'current)
                    (cursor-line-length-cells-before-cursor (old-cursor-line))
                    line-index))
         (forward? (equal? direction 'forward))
         (cursor-line-to-be (if forward?
                                (car old-after)
                                (car old-before)))
         (new-cursor-line (normal-line->cursor-line cursor-line-to-be index))
         (new-lines-before (if forward?
                               (cons old-cursor-line-normalized old-before)
                               (cdr old-before)))
         (new-lines-after (if forward?
                              (cdr old-after)
                              (cons old-cursor-line-normalized old-after)))
         (new-n-before (+ (fun-terminal-length-lines-before-cursor terminal)
                          (if forward? 1 -1)))
         (new-n-after (+ (fun-terminal-length-lines-after-cursor terminal)
                          (if forward? -1 1))))
    (make-fun-terminal new-lines-before new-lines-after new-cursor-line new-n-before new-n-after)))

(define (line-break-at-cursor terminal)
  (let* ((old-cursor-line (fun-terminal-line-with-cursor terminal))
         (new-lines-before (cons (cursor-line-cells-before-cursor old-cursor-line)
                                 (fun-terminal-lines-before-cursor terminal)))
         (new-cursor-line (make-cursor-line '()
                                            (cursor-line-cells-after-cursor old-cursor-line)
                                            0)))
    (struct-copy fun-terminal terminal
                 [lines-before-cursor new-lines-before]
                 [length-lines-before-cursor
                  (+ 1 (fun-terminal-length-lines-before-cursor terminal))]
                 [line-with-cursor new-cursor-line])))

;; TODO - join previous line to cursor line

(define (fun-terminal-insert-at-cursor terminal cell)
  (if (equal? (cell-character cell) #\newline)
      (line-break-at-cursor terminal)
      (let ((old-cursor-line (fun-terminal-line-with-cursor terminal)))
        (struct-copy fun-terminal terminal
                     [line-with-cursor (cursor-line-insert-cell
                                        old-cursor-line cell)]))))

(define (fun-terminal-delete-backwards-at-cursor terminal)
  ;; TODO - what about deleting at the beginning?  Error, or join lines?
  (let ((old-cursor-line (fun-terminal-line-with-cursor terminal)))
    (struct-copy fun-terminal terminal
                 [line-with-cursor (cursor-line-delete-cell-backward old-cursor-line)])))

(define (fun-terminal->lines-from-end terminal)
  ;; gives the lines in reverse order, because the last lines will be the ones used first
  (foldl cons
         (fun-terminal-lines-before-cursor terminal)
         (cons (cursor-line->normal-line (fun-terminal-line-with-cursor terminal))
               (fun-terminal-lines-after-cursor terminal))))

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
