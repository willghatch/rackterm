#lang racket/base

;; This is a functional data structure to hold the cells of the terminal emulator.

(require "cell.rkt")
(require racket/list)

(provide
 (all-defined-out)
 )


(define-struct cursor-line
  (cells-before-cursor ; reversed list
   cells-after-cursor ; non-reversed list
   length-cells-before-cursor) ; AKA cursor position
  #:transparent)
(define the-empty-cursor-line
  (make-cursor-line '() '() 0))

(define-struct fun-terminal
  (pre-cursor-lines ; reversed from normal order
   post-cursor-lines
   cursor-line
   pre-cursor-lines-length
   post-cursor-lines-length))
(define the-empty-fun-terminal
  (make-fun-terminal '() '() the-empty-cursor-line 0 0))

(define (cursor-line->normal-line line [style-cursor? #f])
  (define (cursor-style c)
    (let ((s (cell-style c)))
      (make-cell (cell-character c)
                 (struct-copy style s
                              [reverse-video (not (style-reverse-video s))]))))
  (let* ((after-orig (cursor-line-cells-after-cursor line))
         (after-mod (if style-cursor?
                        (if (null? after-orig)
                            (list (cursor-style blank-cell))
                            (cons (cursor-style (car after-orig))
                                  (cdr after-orig)))
                        after-orig)))
    (foldl cons after-mod (cursor-line-cells-before-cursor line))))

(define (normal-line->cursor-line line [line-index 0])
  (let* ((len (length line))
         (extended-line (if (line-index . <= . len)
                            line
                            (append line (make-list (line-index . - . len)
                                                     blank-cell))))
         (new-len (max len line-index))
         (end (list-tail extended-line line-index))
         (beg (list-tail (reverse extended-line) (- new-len line-index))))
    (make-cursor-line beg end line-index)))

(define (cursor-line-delete-cell-forward line [n 1])
  (let* ((old-after (cursor-line-cells-after-cursor line))
         (new-after (if ((length old-after) . <= . n)
                        '()
                        (list-tail old-after n))))
    (struct-copy cursor-line line
                 [cells-after-cursor new-after])))

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

(define (cursor-line-clear-start-to-cursor line)
  (struct-copy cursor-line line
               [cells-before-cursor (make-list (length (cursor-line-cells-before-cursor line))
                                               blank-cell)]))

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

(define (cursor-line-move-cursor-forward line [additive? #t])
  (let ((after (cursor-line-cells-after-cursor line)))
    (cond ((and (null? after)
                (not additive?))
           line)
          ((null? after)
           (cursor-line-insert-cell line blank-cell))
          (else
           (make-cursor-line (cons (car after)
                                   (cursor-line-cells-before-cursor line))
                             (cdr after)
                             (add1 (cursor-line-length-cells-before-cursor line)))))))

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
  (let* ((old-before (fun-terminal-pre-cursor-lines terminal))
         (old-after (fun-terminal-post-cursor-lines terminal)))
    (cond
      [(or (and forward? (null? old-after) (not additive?))
           (and (not forward?) (null? old-before)))
       terminal]
      [(and forward? (null? old-after) additive?)
       (fun-terminal-line-break terminal)]
      [else
        terminal
        (let* ((old-cursor-line (fun-terminal-cursor-line terminal))
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
               (new-n-before (+ (fun-terminal-pre-cursor-lines-length terminal)
                                (if forward? 1 -1)))
               (new-n-after (+ (fun-terminal-post-cursor-lines-length terminal)
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
                                  (fun-terminal-cursor-line terminal))
                                 (fun-terminal-pre-cursor-lines terminal)))
         (new-cursor-line the-empty-cursor-line))
    (struct-copy fun-terminal terminal
                 [pre-cursor-lines new-lines-before]
                 [pre-cursor-lines-length
                  (add1 (fun-terminal-pre-cursor-lines-length terminal))]
                 [cursor-line new-cursor-line])))

(define (fun-terminal-line-break-at-cursor terminal)
  (let* ((old-cursor-line (fun-terminal-cursor-line terminal))
         (new-lines-before (cons (cursor-line-cells-before-cursor old-cursor-line)
                                 (fun-terminal-pre-cursor-lines terminal)))
         (new-cursor-line (make-cursor-line '()
                                            (cursor-line-cells-after-cursor old-cursor-line)
                                            0)))
    (struct-copy fun-terminal terminal
                 [pre-cursor-lines new-lines-before]
                 [pre-cursor-lines-length
                  (+ 1 (fun-terminal-pre-cursor-lines-length terminal))]
                 [cursor-line new-cursor-line])))

(define (fun-terminal-add-blank-line-at-end term)
  (struct-copy fun-terminal term
               [post-cursor-lines (append (fun-terminal-post-cursor-lines term) '(()))]
               [post-cursor-lines-length (add1 (fun-terminal-post-cursor-lines-length term))]))

(define (fun-terminal-edit-cursor-line term cl-func . cl-args)
  (struct-copy fun-terminal term
               [cursor-line (apply cl-func
                                        (cons (fun-terminal-cursor-line term)
                                              cl-args))]))

(define (fun-terminal-insert-at-cursor terminal cell)
  (fun-terminal-edit-cursor-line terminal cursor-line-insert-cell cell))

(define (fun-terminal-delete-backwards-at-cursor terminal)
  (fun-terminal-edit-cursor-line terminal cursor-line-delete-cell-backward))
(define (fun-terminal-delete-forward-at-cursor terminal [n 1])
  (fun-terminal-edit-cursor-line terminal cursor-line-delete-cell-forward n))
(define (fun-terminal-overwrite terminal cell)
  (fun-terminal-edit-cursor-line terminal cursor-line-overwrite cell))

(define (fun-terminal-forward-cells term [n-cells 1])
  (struct-copy fun-terminal term
               [cursor-line (cursor-line-advance-cursor
                                  (fun-terminal-cursor-line term)
                                  n-cells)]))

(define (fun-terminal-delete-to-end-of-line term)
  (struct-copy fun-terminal term
               [cursor-line (struct-copy cursor-line (fun-terminal-cursor-line term)
                                              [cells-after-cursor '()])]))
(define (fun-terminal-clear-from-start-of-line-to-cursor term)
  (fun-terminal-edit-cursor-line term cursor-line-clear-start-to-cursor))

(define (fun-terminal-delete-post-cursor-lines term)
  (struct-copy fun-terminal term
               [post-cursor-lines '()]
               [post-cursor-lines-length 0]))

(define (fun-terminal-delete-n-pre-cursor-lines term n)
  (if ((fun-terminal-pre-cursor-lines-length term) . < . n)
      (struct-copy fun-terminal term
                   [pre-cursor-lines '()]
                   [pre-cursor-lines-length 0])
      (struct-copy fun-terminal term
                   [pre-cursor-lines (list-tail (fun-terminal-pre-cursor-lines term) n)]
                   [pre-cursor-lines-length (- (fun-terminal-pre-cursor-lines-length term) n)])))

(define (fun-terminal-clear-line term)
  (struct-copy fun-terminal term
               [cursor-line the-empty-cursor-line]))

(define (fun-terminal->lines-from-end terminal [style-cursor? #f])
  ;; gives the lines in reverse order, because the last lines will be the ones used first
  (foldl cons
         (fun-terminal-pre-cursor-lines terminal)
         (cons (cursor-line->normal-line (fun-terminal-cursor-line terminal) style-cursor?)
               (fun-terminal-post-cursor-lines terminal))))

(define (fun-terminal-get-column term)
  (cursor-line-length-cells-before-cursor (fun-terminal-cursor-line term)))

(define (fun-terminal-get-rows-from-end term)
  (fun-terminal-post-cursor-lines-length term))

(define (fun-terminal-get-num-rows term)
  (+ (fun-terminal-post-cursor-lines-length term)
     (fun-terminal-pre-cursor-lines-length term)
     1))

(define (fun-terminal-scroll-region term n-pre n-post n-scrolls)
  (define (-scroll c-line before-lines after-lines n-before n-after n-scrolls)
    (let* ((region (append (reverse (take before-lines n-before))
                           (cons c-line (take after-lines n-after))))
           (new-region (append (list-tail region n-scrolls)
                               (make-list n-scrolls '())))
           (new-before (append (reverse (take new-region n-before))
                               (list-tail before-lines n-before)))
           (new-c-line (list-ref new-region n-before))
           (new-after (append (list-tail new-region (add1 n-before))
                              (list-tail after-lines n-after))))
      (values new-before new-c-line new-after)))
  (let* ((forward? (positive? n-scrolls))
         (before-lines (if forward? (fun-terminal-pre-cursor-lines term)
                           (fun-terminal-post-cursor-lines term)))
         (after-lines (if forward? (fun-terminal-post-cursor-lines term)
                          (fun-terminal-pre-cursor-lines term)))
         (nn-pre (if forward? n-pre n-post))
         (nn-post (if forward? n-post n-pre))
         (c-line (fun-terminal-cursor-line term))
         (c-line-index (length (cursor-line-cells-before-cursor c-line)))
         (norm-line (cursor-line->normal-line (fun-terminal-cursor-line term))))
    (define-values (new-before new-c-line new-after)
      (-scroll norm-line before-lines after-lines nn-pre nn-post (abs n-scrolls)))
    (struct-copy fun-terminal term
                 [pre-cursor-lines (if forward? new-before new-after)]
                 [post-cursor-lines (if forward? new-after new-before)]
                 [cursor-line (normal-line->cursor-line new-c-line c-line-index)])))
