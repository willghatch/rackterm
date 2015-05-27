#lang racket/base
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
; Maybe a reversed list, so characters at the
; end of the line can be cons-ed on

(define-struct terminal (line-list) #:mutable) ; again, reversed list

(define (make-new-terminal)
  (make-terminal '(()) ))

(define (terminal-append-line terminal line)
  (let ((new-lines (cons line (terminal-line-list terminal))))
    (set-terminal-line-list! terminal new-lines)))

(define (terminal-append-cell terminal cell)
  (if (or (equal? (cell-character cell) #\newline) (equal? (cell-character cell) #\return))
      (terminal-append-line terminal null)
      (let* ((lines (terminal-line-list terminal))
             (new-last-line (cons cell (car lines)))
             (new-lines (cons new-last-line (cdr lines))))
        (set-terminal-line-list! terminal new-lines))))

(provide (all-defined-out))
