#lang racket/base

;; This is to be executed, running a GUI terminal emulator

(require racket/class)
(require racket/gui/base)
(require "terminal.rkt")

;;; YARR!!! Here be the ugliest code in the whole project!  This file is a mess!

;;; TODO:
;;; - clean this crap up
;;; - add customization to set the 16 color palette map, input mapping
;;; - do key input mapping (arrow keys, etc)
;;; - do cursor better...
;;; - make it faster -- currently if anything on a line is different I redraw the line.
;;;   I really should just redraw any cells that are different instead
;;; - wrap input that is too long after a resize down to fewer columns...

(define my-terminal (init-terminal (lambda ()
                                     (send the-canvas refresh))
                                   (or (getenv "SHELL")
                                       "/bin/sh")
                                   "-i"))

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
    (define last-lines '())
    (define last-bitmap (make-object bitmap% 500 500))
    (define last-dc (make-object bitmap-dc% last-bitmap))
    (define (resize-maybe width height)
      (if (or (not (equal? width last-width))
              (not (equal? height last-height)))
          (begin
            (set! last-width width)
            (set! last-height height)
            (terminal-set-size terminal width height)
            #t)
          #f))

    (resize-maybe 80 24) ; call at startup so it initializes well

    (define (get-text-width-height)
      (define-values (width height _ __) (send (send this get-dc) get-text-extent "a"))
      (values width height))

    (define (cell-size)
      (send this get-cell-size (make-cell #\@ default-style)))
    (define/public (get-xterm-size)
      (define-values (c-width c-height) (cell-size))
      (define-values (x-size y-size) (send (send this get-dc) get-size))
      (define (trunc num) (inexact->exact (truncate num)))
      (values (trunc (/ x-size c-width)) (trunc (/ y-size c-height))))

    (define/public (get-cell-size cell)
      ;; todo -- add font...
      (let-values [((width height _ __)
                    (send last-dc
                          get-text-extent
                          (string (cell-character cell))))]
        (values width height)))

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
        (define-values (c-width c-height) (cell-size))
        (values (* c-width (length line)) c-height))
      (define (print-terminal-line dc line really-print?)
        ;; ok, so since this has extra side effects I need to fake print it either way
        (define-values (line-width line-height) (get-line-size line))
        (define-values (pixel-x-size pixel-y-size) (send dc get-size))
          (set! cur-y (- cur-y line-height))
          (set! cur-x 0)
          (when really-print?
            (send dc set-brush "black" 'solid)
            (send dc set-pen "black" 0 'solid)
            (send dc draw-rectangle cur-x cur-y pixel-x-size line-height)
            (for [(cell line)]
              (print-terminal-cell dc cell))))
      (define (print-terminal-cell dc cell)
        (define s (cell-style cell))
        (send dc set-font (send the-font-list find-or-create-font
                                12
                                ;"DejaVu Sans Mono"
                                'modern ; default, decorative, roman, script, swiss, modern, symbol, system
                                (if (style-italic s) 'italic 'normal) ; normal, italic, slant
                                (if (style-bold s) 'bold 'normal) ; normal, bold, light
                                (style-underline s) ; underline?
                                ))
        (send dc set-text-background (style->color% s #f))
        (send dc set-text-foreground (style->color% s #t))
        (send dc draw-text (string (cell-character cell)) cur-x cur-y)
        (define-values (cell-width cell-height) (get-cell-size cell))
        (set! cur-x (+ cur-x cell-width)))

      (define lines (terminal-get-lines terminal))
      (define (repaint all?)
        ;; clear to start painting again...
        (when all?
          (send dc clear)
          (define-values (x-size y-size) (send (send this get-dc) get-size))
          (printf "sizes: ~s ~s~n" x-size y-size)
          (set! last-bitmap (make-object bitmap% x-size y-size))
          (set! last-dc (make-object bitmap-dc% last-bitmap))
          (send last-dc set-background "black")
          (send last-dc clear)
          (send last-dc set-text-mode 'solid)
          )

        (for [(line lines)
              (ol last-lines)
              #:break (< cur-y 0)]
          (if (or all? (not (equal? line ol)))
              (print-terminal-line last-dc line #t)
              (print-terminal-line last-dc line #f))
        (send last-dc flush)))

      (define resized? (resize-maybe xterm-width xterm-height))
      (if (or resized? (null? last-dc))
          (repaint #t)
          (repaint #f))

      (send dc draw-bitmap last-bitmap 0 0)
      (set! last-lines lines)
      (send dc flush)) ;; end on-paint


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
       ))




(send the-canvas focus)
(define the-dc (send the-canvas get-dc))

