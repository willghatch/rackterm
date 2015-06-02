#lang racket/base
(require racket/class)
(require racket/gui/base)
(require "terminal.rkt")

(define frame (new frame%
                   [label "racket xterm"]
                   [width 100]
                   [height 100]))

(send frame show #t)


(define terminal-canvas%
  (class canvas%
    (init-field terminal)
    (define/public (get-terminal) terminal)

    (define (get-text-width-height)
      (define-values (width height _ __) (send (send this get-dc) get-text-extent "a"))
      (values width height))

    (define/public (get-terminal-size)
      (define cell-size (send this get-cell-size (cell #\a "white" "black" '())))
      (define-values (x-size y-size) (send (send this get-dc) get-size))
      (values (floor (/ x-size (car cell-size))) (floor (/ y-size (cadr cell-size)))))

    (define/override (on-paint)
      (define dc (send this get-dc))
      ;; I need to keep track of the coordinates and only draw lines inside the max
      (define-values (x-size y-size) (send dc get-size))
      (define cur-x 0)
      (define cur-y y-size) ; start drawing at the bottom left
      ;; How do you just get one value and ignore the others?
      (define (get-cell-size cell)
        ;; todo -- add font...
        (let-values [((width height _ __)
                      (send (send this get-dc)
                            get-text-extent
                            (string (cell-character cell))))]
          (list width height)))
      (define-values (current-font-width current-font-height) (get-text-width-height))
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
        (send dc set-text-background (cell-bg-color cell))
        (send dc set-text-foreground (cell-fg-color cell))
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

      (for [(line (terminal-get-lines terminal))
            #:break (< cur-y 0)]
        (print-terminal-line line))

      (send dc flush))

    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (if (char? key)
            (for ((char (map-event-to-terminal-codes event)))
              (send-char-to-terminal-process terminal char))
            null))
      (send this on-paint))

    (super-new)
    ;; start thread to listen for input from the subprocess
    (thread (terminal-input-listener terminal))
    ))

(define (control-version key)
  ;; return what the key would be if control were held down
  (define as-int (char->integer key))
  (if (as-int . < . 128)
      (integer->char (bitwise-and 31 as-int))
      key))

(define (map-event-to-terminal-codes event)
  (let* ((key (send event get-key-code))
         (ctl (send event get-control-down))
         (meta (send event get-meta-down))
         (alt (send event get-alt-down))
         (m3 (send event get-mod3-down))
         (m4 (send event get-mod4-down))
         (m5 (send event get-mod5-down))
         (key-with-ctl (if ctl (control-version key) key)))
    ;; note, there is also a C+M=altr option here...
    ;; some day I'll have some table lookup for extra values...
    ;(printf "key: ~a, ctl: ~a, alt: ~a, meta: ~a" key ctl alt meta)
    (if (or alt meta)
        (list #\033 key-with-ctl)
        (list key-with-ctl))))

(define the-canvas
  (new terminal-canvas%
       [terminal (init-terminal "setsid bash -i -"
                                        (lambda ()
                                          (send the-canvas refresh)))]
       [parent frame]
       ;[style '(no-autoclear)]
       ))




(send the-canvas focus)
(define the-dc (send the-canvas get-dc))

