#lang racket/base

(provide
 make-empty-fun-terminal
 fun-terminal->lines-from-end
 fun-terminal-insert-at-cursor
 fun-terminal-delete-backwards-at-cursor
 fun-terminal-line-break-at-cursor
 fun-terminal-forward-lines
 fun-terminal-forward-cells
 fun-terminal-get-column
 fun-terminal-get-rows-from-end
 fun-terminal-get-num-rows
 fun-terminal-delete-to-end-of-line
 fun-terminal-clear-line
 fun-terminal-overwrite
 (struct-out cell)
 )

;; Cells - each cell has RGBA color, background, a unicode character, attributes such as underline, blink, italic... similar to emacs face properties.  Maybe font?  Cell size?  Images could be large cells?
(define-struct cell
  (character fg-color bg-color attr-list)) ; maybe just have a list of attrs, so
  ; the average cell doesn't have as much data...
(define blank-cell (make-cell #\space 'default 'default '()))

;; lines are a series of cells.  I should have normal mode output keep in one line and wrap it according to terminal size (and re-wrapping on size change)

(define-struct cursor-line
  (cells-before-cursor ; reversed list
   cells-after-cursor ; non-reversed list
   length-cells-before-cursor)) ; AKA cursor position
(define (make-empty-cursor-line)
  (make-cursor-line '() '() 0))

(define-struct fun-terminal
  (lines-before-cursor ; reversed from normal order
   lines-after-cursor
   line-with-cursor
   length-lines-before-cursor
   length-lines-after-cursor))
(define (make-empty-fun-terminal)
  (make-fun-terminal '() '() (make-empty-cursor-line) 0 0))

(define (cursor-line->normal-line line)
  (foldl cons (cursor-line-cells-before-cursor line) (cursor-line-cells-after-cursor line)))

(define (normal-line->cursor-line line [line-index 0])
  (let* ((len (length line))
         (extended-line (if (line-index . <= . len)
                            line
                            (append line (build-list (line-index . - . len)
                                                     (lambda (x) blank-cell)))))
         (end (list-tail extended-line line-index))
         (beg (reverse (for/list ([elem extended-line]
                                  [i (in-naturals)]
                                  #:break (i . >= . line-index))
                         elem))))
    #;(when (not (equal? line extended-line))
        (printf "pre: ~a~npost: ~a~n" (map cell-character line) (map cell-character extended-line)))
    (make-cursor-line beg end line-index)))

(define (cursor-line-delete-cell-forward line)
  (if (null? (cursor-line-cells-after-cursor line))
      line
      (struct-copy cursor-line line
                   [cells-after-cursor (cdr (cursor-line-cells-after-cursor line))])))
(define (cursor-line-delete-cell-backward line)
  (if (equal? (cursor-line-length-cells-before-cursor line) 0)
      line
      (struct-copy cursor-line line
                   [cells-before-cursor
                    (cdr (cursor-line-cells-before-cursor line))]
                   [length-cells-before-cursor
                    (sub1 (cursor-line-length-cells-before-cursor line))])))

(define (cursor-line-insert-cell line cell)
  (struct-copy cursor-line line
               [cells-before-cursor (cons cell (cursor-line-cells-before-cursor line))]
               [length-cells-before-cursor (add1 (cursor-line-length-cells-before-cursor line))]))

(define (cursor-line-overwrite line cell)
  (cursor-line-insert-cell (cursor-line-delete-cell-forward line)
                           cell))

(define (cursor-line-move-cursor-backward line)
  (if (equal? (cursor-line-length-cells-before-cursor line) 0)
      line
      (make-cursor-line (cdr (cursor-line-cells-before-cursor line))
                        (cons (car (cursor-line-cells-before-cursor line))
                              (cursor-line-cells-after-cursor line))
                        (sub1 (cursor-line-length-cells-before-cursor line)))))

(define (cursor-line-move-cursor-forward line)
  (if (null? (cursor-line-cells-after-cursor line))
      line
      (make-cursor-line (cons (car (cursor-line-cells-after-cursor line))
                                   (cursor-line-cells-before-cursor line))
                        (cdr (cursor-line-cells-after-cursor line))
                        (add1 (cursor-line-length-cells-before-cursor line)))))

(define (cursor-line-advance-cursor line [n 1])
  (let ((adv-func (if (positive? n)
                      cursor-line-move-cursor-forward
                      cursor-line-move-cursor-backward)))
    (define (iter line n)
      (if (equal? 0 n)
          line
          (iter (adv-func line) (sub1 n))))
    (iter line (abs n))))


(define (move-cursor-line terminal [forward? #t] [additive? #t] [line-index 'current])
  (let* ((old-before (fun-terminal-lines-before-cursor terminal))
         (old-after (fun-terminal-lines-after-cursor terminal)))
    (cond
      [(or (and forward? (null? old-after) (not additive?))
           (and (not forward?) (null? old-before)))
       terminal]
      [(and forward? (null? old-after) additive?)
       (fun-terminal-line-break terminal)]
      [else
        terminal
        (let* ((old-cursor-line (fun-terminal-line-with-cursor terminal))
               (old-cursor-line-normalized (cursor-line->normal-line old-cursor-line))
               (index (if (equal? line-index 'current)
                          (cursor-line-length-cells-before-cursor old-cursor-line)
                          line-index))
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
          (make-fun-terminal new-lines-before new-lines-after new-cursor-line new-n-before new-n-after))])))

(define (fun-terminal-forward-lines term [n-lines 1])
  (let ((forward? (if (positive? n-lines)
                       #t
                       #f)))
    (define (inner-advance t n)
      (if (equal? 0 n)
          t
          (inner-advance (move-cursor-line t forward?) (sub1 n))))
    (inner-advance term (abs n-lines))))

(define (fun-terminal-line-break terminal)
  (let* ((new-lines-before (cons (cursor-line->normal-line
                                  (fun-terminal-line-with-cursor terminal))
                                 (fun-terminal-lines-before-cursor terminal)))
         (new-cursor-line (make-empty-cursor-line)))
    (struct-copy fun-terminal terminal
                 [lines-before-cursor new-lines-before]
                 [length-lines-before-cursor
                  (add1 (fun-terminal-length-lines-before-cursor terminal))]
                 [line-with-cursor new-cursor-line])))

(define (fun-terminal-line-break-at-cursor terminal)
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

(define (fun-terminal-edit-cursor-line term cl-func . cl-args)
  (struct-copy fun-terminal term
               [line-with-cursor (apply cl-func
                                        (cons (fun-terminal-line-with-cursor term)
                                              cl-args))]))

(define (fun-terminal-insert-at-cursor terminal cell)
  (fun-terminal-edit-cursor-line terminal cursor-line-insert-cell cell))

(define (fun-terminal-delete-backwards-at-cursor terminal)
  (fun-terminal-edit-cursor-line terminal cursor-line-delete-cell-backward))
(define (fun-terminal-delete-forward-at-cursor terminal)
  (fun-terminal-edit-cursor-line terminal cursor-line-delete-cell-forward))
(define (fun-terminal-overwrite terminal cell)
  (fun-terminal-edit-cursor-line terminal cursor-line-overwrite cell))

(define (fun-terminal-forward-cells term [n-cells 1])
  (struct-copy fun-terminal term
               [line-with-cursor (cursor-line-advance-cursor
                                  (fun-terminal-line-with-cursor term)
                                  n-cells)]))

(define (fun-terminal-delete-to-end-of-line term)
  (struct-copy fun-terminal term
               [line-with-cursor (struct-copy cursor-line (fun-terminal-line-with-cursor term)
                                              [cells-after-cursor '()])]))
(define (fun-terminal-clear-line term)
  (struct-copy fun-terminal term
               [line-with-cursor (make-empty-cursor-line)]))

(define (fun-terminal->lines-from-end terminal)
  ;; gives the lines in reverse order, because the last lines will be the ones used first
  (foldl cons
         (fun-terminal-lines-before-cursor terminal)
         (cons (cursor-line->normal-line (fun-terminal-line-with-cursor terminal))
               (fun-terminal-lines-after-cursor terminal))))

(define (fun-terminal-get-column term)
  (cursor-line-length-cells-before-cursor (fun-terminal-line-with-cursor term)))

(define (fun-terminal-get-rows-from-end term)
  (fun-terminal-lines-after-cursor term))

(define (fun-terminal-get-num-rows term)
  (+ (fun-terminal-lines-after-cursor term)
     (fun-terminal-lines-before-cursor term)
     1))
