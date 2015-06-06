#lang racket/base
(require racket/class)
(require racket/gui/base)
(require "terminal.rkt")

(define my-terminal (init-terminal-with-shell-trampoline (lambda ()
                                      (send the-canvas refresh))
                                    (or (getenv "SHELL")
                                        "/bin/sh")
                                    "-i"))
;; TODO - move this into the initialization function...
(terminal-set-default-tab-stops my-terminal)

(define frame (new frame%
                   [label "racket xterm"]
                   [width 800]
                   [height 800]))

(send frame show #t)


(define terminal-canvas%
  (class canvas%
    (init-field terminal)
    (define/public (get-terminal) terminal)

    (define last-width 0)
    (define last-height 0)

    (define (resize-maybe width height)
      (when (or (not (equal? width last-width))
                (not (equal? height last-height)))
        (begin
          (set! last-width width)
          (set! last-height height)
          (terminal-set-size terminal width height))))
    (resize-maybe 80 24) ; call at startup so it initializes well

    (define (get-text-width-height)
      (define-values (width height _ __) (send (send this get-dc) get-text-extent "a"))
      (values width height))

    (define/public (get-xterm-size)
      (define cell-size (send this get-cell-size (cell #\a "white" "black" '())))
      (define-values (x-size y-size) (send (send this get-dc) get-size))
      (define (trunc num) (inexact->exact (truncate num)))
      (values (trunc (/ x-size (car cell-size))) (trunc (/ y-size (cadr cell-size)))))

    (define/public (get-cell-size cell)
      ;; todo -- add font...
      (let-values [((width height _ __)
                    (send (send this get-dc)
                          get-text-extent
                          (string (cell-character cell))))]
        (list width height)))

    (define/override (on-paint)
      (send frame set-label (terminal-title terminal))
      (define dc (send this get-dc))
      ;; I need to keep track of the coordinates and only draw lines inside the max
      (define-values (x-size y-size) (send dc get-size))
      (define cur-x 0)
      (define cur-y y-size) ; start drawing at the bottom left
      ;; How do you just get one value and ignore the others?
      (define-values (current-font-width current-font-height) (get-text-width-height))
      (define-values (xterm-width xterm-height) (send this get-xterm-size))
      (define (get-line-size line)
        (let* ((cell-sizes (map (lambda (l) (send this get-cell-size l)) line))
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
          (for [(cell line)]
            (print-terminal-cell cell))))
      (define (print-terminal-cell cell)
        (send dc set-text-background (cell-bg-color cell))
        (send dc set-text-foreground (cell-fg-color cell))
        (send dc draw-text (string (cell-character cell)) cur-x cur-y)
        (set! cur-x (+ cur-x (car (get-cell-size cell)))))


      (resize-maybe xterm-width xterm-height)

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
            null)))

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
         (key-with-ctl (if ctl (control-version key) key))
         (key-backspace-hack (if (equal? key-with-ctl #\u08)
                                 #\u7F
                                 key-with-ctl))
         )
    ;; note, there is also a C+M=altr option here...
    ;; some day I'll have some table lookup for extra values...
    ;(printf "key: ~a, ctl: ~a, alt: ~a, meta: ~a" key ctl alt meta)
    (if (or alt meta)
        (list #\033 key-backspace-hack)
        (list key-backspace-hack))))

(define the-canvas
  (new terminal-canvas%
       [terminal my-terminal]
       [parent frame]
       ;[style '(no-autoclear)]
       ))




(send the-canvas focus)
(define the-dc (send the-canvas get-dc))

