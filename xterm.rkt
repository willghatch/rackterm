#lang racket/base
(require racket/class)
(require racket/gui/base)
(require "terminal.rkt")

; Make a frame by instantiating the frame% class
(define frame (new frame%
                   [label "racket xterm"]
                   [width 100]
                   [height 100]))

; Show the frame by calling its show method
(send frame show #t)


#;(define msg (new message% [parent frame]
                 [label "No events so far..."]))

; Derive a new canvas (a drawing window) class to handle events
;(define my-canvas%
;  (class canvas% ; The base class is canvas%
;    ; Define overriding method to handle mouse events
;    (define/override (on-event event)
;      (send msg set-label "Canvas mouse"))
;    ; Define overriding method to handle keyboard events
;    (define/override (on-char event)
;      (send msg set-label "Canvas keyboard"))
;    ; Call the superclass init, passing on all init args
;    (super-new)))

; Make a canvas that handles events in the frame
;(new my-canvas% [parent frame])
(define terminal-canvas%
  (class canvas%
    (init-field terminal)
    (define/override (on-paint)
      (define dc (send this get-dc))
      ;; I need to keep track of the coordinates and only draw lines inside the max
      (define-values (x-size y-size) (send dc get-size))
      (define cur-x 0)
      (define cur-y y-size) ; start drawing at the bottom left
      (define (get-cell-size cell)
        ;; todo -- add font...
        (let-values [((width height _ __)
                     (send dc get-text-extent (string (cell-character cell))))]
          (list width height)))
      ;; How do you just get one value and ignore the others?
      (define-values (_ current-font-height _- -_) (send dc get-text-extent "a"))
      (define (get-line-size line)
        (let* ((cell-sizes (map get-cell-size line))
               ;; what is the built-in sum function called?
               (width (foldl (lambda (x sum) (+ x sum)) 0 (map car cell-sizes)))
               (height (apply max (cons current-font-height (map cadr cell-sizes)))))
          (list width height)))
      (define (print-terminal-line line)
        (let* ((line-size (get-line-size line))
               (line-width (car line-size))
               (line-height (cadr line-size)))
          (set! cur-y (- cur-y line-height))
          (set! cur-x 0)
          (for [(cell (reverse line))]
            (print-terminal-cell cell))))
      (define (print-terminal-cell cell)
        (send dc draw-text (string (cell-character cell)) cur-x cur-y)
        (set! cur-x (+ cur-x (car (get-cell-size cell)))))

      ;; clear to start painting again...
      (send dc set-background "black")
      (send dc clear)
      (send dc set-font (send the-font-list find-or-create-font
                              12
                              ;"DejaVu Sans Mono"
                              'modern ; default, decorative, roman, script, swiss, modern, symbol, system
                              'normal ; normal, italic, slant
                              'normal ; normal, bold, light
                              #f ; underline?
                              ))

      (send dc set-text-mode 'solid) ; use the background color...
      (send dc set-text-background "black")
      (send dc set-text-foreground "white")

      (for [(line (fun-terminal->lines-from-end terminal))
            #:break (< cur-y 0)]
        (print-terminal-line line))

      (send dc flush))

    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (if (char? key)
            (if (equal? key #\return)
                (set-field! terminal this (line-break-at-cursor terminal))
                (set-field! terminal this (insert-at-cursor terminal (make-cell (send event get-key-code) 'foo-color 'bar-color '()))))
            null))
      (send this on-paint))

    (super-new)
    ))


(define the-canvas (new terminal-canvas%
                        [terminal (make-empty-fun-terminal)]
                        [parent frame]
                        ;[style '(no-autoclear)]
                        ))
(send the-canvas focus)
(define the-dc (send the-canvas get-dc))

