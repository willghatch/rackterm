#lang racket/base

(require racket/gui/base
         racket/class)
(require "terminal.rkt")
(require "key-tree.rkt")

(provide terminal-canvas%
         )

;;; YARR!!! Here be the ugliest code in the whole project!  This file is a mess!

;;; TODO:
;;; - clean this crap up
;;; - add customization to set the 16 color palette map, input mapping
;;; - do key input mapping (arrow keys, etc)
;;; - do cursor better...
;;; - make it faster -- currently if anything on a line is different I redraw the line.
;;;   I really should just redraw any cells that are different instead
;;; - wrap input that is too long after a resize down to fewer columns...

(define terminal-canvas%
  (class canvas%
    (init-field [command-and-args (list (or (getenv "SHELL") "/bin/sh") "-i")])
    (init-field [term-var "rackterm"])
    (init-field [terminal
                 (init-terminal (lambda () (send this refresh))
                                (lambda () (send this handle-subproc-ended))
                                (if (null? command-and-args)
                                    (list (or (getenv "SHELL")
                                              "/bin/sh")
                                          "-i")
                                    command-and-args)
                                #:term-var term-var)])

    (define/public (get-terminal) terminal)

    (init-field [set-title-callback void])
    (define (set-parent-title)
      (when (send this has-focus?)
        (set-title-callback (terminal-title terminal))))
    (define/override (on-focus event)
      (set-parent-title))

    (init-field [font-size 12])
    (define/public (get-font-size) font-size)
    (define/public (set-font-size! size) (set! font-size size))

    (init-field [font-fallback-list
                 '("DejaVu Sans Mono"
                   "Ubuntu Mono"
                   "Droid Sans Mono"
                   "Liberation Mono"
                   "Terminal"
                   "Menlo"
                   "Monaco"
                   "Courier"
                   "Courier New")])
    (define/public (get-font-fallback-list) font-fallback-list)
    (define/public (set-font-fallback-list fonts) (set! font-fallback-list fonts))
    (define (get-first-available-font fonts)
      (cond [(null? fonts) #f]
            [(member (car fonts) (get-face-list)) (car fonts)]
            [else (get-first-available-font (cdr fonts))]))

    (init-field [font-name "use first fallback"])
    (define/public (get-font-name) font-name)
    (define/public (set-font-name! f)
      (set! font-name
            (get-first-available-font (cons f font-fallback-list))))
    ;; set the font explicitly to trigger fallback behavior
    (send this set-font-name! (send this get-font-name))

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

    (define (set-dc-font dc style)
      (send dc set-font (send the-font-list find-or-create-font
                              (send this get-font-size)
                              (send this get-font-name)
                              'modern ; default, decorative, roman, script, swiss, modern, symbol, system
                              (if (style-italic style) 'italic 'normal) ; normal, italic, slant
                              (if (style-bold style) 'bold 'normal) ; normal, bold, light
                              (style-underline style) ; underline?
                              )))

    (define (cell-size)
      (define (get-cell-size cell)
        (set-dc-font last-dc (cell-style cell))
        (let-values [((width height _ __)
                      (send last-dc
                            get-text-extent
                            (string (cell-character cell))))]
          (values width height)))
      (get-cell-size (make-cell #\@ default-style)))
    (define/public (get-xterm-size)
      (define-values (c-width c-height) (cell-size))
      (define-values (x-size y-size) (send (send this get-dc) get-size))
      (define (trunc num) (inexact->exact (truncate num)))
      (values (trunc (/ x-size c-width)) (trunc (/ y-size c-height))))


    (define/override (on-paint)
      (send this set-label (terminal-title terminal))
      (set-parent-title)
      (define dc (send this get-dc))
      ;; I need to keep track of the coordinates and only draw lines inside the max
      (define-values (x-size y-size) (send dc get-size))
      (define cur-x 0)
      (define cur-y y-size) ; start drawing at the bottom left
      ;; How do you just get one value and ignore the others?
      (define-values (current-font-width current-font-height) (get-text-width-height))
      (define-values (xterm-width xterm-height) (send this get-xterm-size))
      (define-values (cell-width cell-height) (cell-size))
      (define (get-line-size line)
        (values (* cell-width (length line)) cell-height))
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
        (set-dc-font dc s)
        (send dc set-text-background (style->color% s #f))
        (send dc set-text-foreground (style->color% s #t))
        (send dc draw-text (string (cell-character cell)) cur-x cur-y)
        (set! cur-x (+ cur-x cell-width)))

      (define lines (terminal-get-lines terminal))
      (define (repaint all?)
        ;; clear to start painting again...
        (when all?
          (send dc clear)
          (define-values (x-size y-size) (send (send this get-dc) get-size))
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

    (define/public (handle-subproc-ended)
      ;; TODO - this should be a parameter that can be set from outside...
      (define parent (send this get-parent))
      (define focused? (send this has-focus?))
      (send parent delete-child this)
      (define others (send parent get-children))
      (for ((o others))
        (send o refresh))
      (if (null? others)
          (exit 0)
          (send (car others) focus))
      (send parent reflow-container)
      )

    (init-field [input-key-tree key-tree-command-map])
    (define current-key-tree input-key-tree)

    (define (handle-event-giving-terminal-codes event)
      (let* ((k-ev (map-char-event-to-key-tree-event event))
             (mapper (key-tree-get current-key-tree
                                   (map-char-event-to-key-tree-event event)))
             (chars (if (key-tree? mapper)
                        (set! current-key-tree mapper)
                        (begin
                          (set! current-key-tree input-key-tree)
                          (mapper k-ev)))))
        (if (sequence? chars) chars '())))

    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (if (char? key)
            (for ((char (handle-event-giving-terminal-codes event)))
              (when (char? char)
                ;(printf "sending to subproc: ~s~n" char)
                (send-char-to-terminal-process terminal char)))
            null)))
    (define/override (on-event event)
      (when (member (send event get-event-type) '(left-down right-down middle-down))
        (send this focus)))

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

(define (map-char-event-to-key-tree-event event)
  (let ((char (send event get-key-code))
        (C (send event get-control-down))
        (M (send event get-meta-down))
        (A (send event get-alt-down))
        ;; G for super
        (G (send event get-mod4-down))
        (H #f))
    (make-key-event char C (or M A) G H)))

(define meta-term-prefix #\033)
(define super-term-prefix #f)
(define hyper-term-prefix #f)

(define (received-key-default-mapper key-ev)
  (let ((meta (key-event-meta key-ev))
        (ctl (key-event-control key-ev))
        (sup (key-event-super key-ev))
        (hyp (key-event-hyper key-ev))
        (char (key-event-char key-ev)))
  `(,@(if (and meta meta-term-prefix) (list meta-term-prefix) '())
    ,@(if (and sup super-term-prefix) (list super-term-prefix) '())
    ,@(if (and hyp hyper-term-prefix) (list hyper-term-prefix) '())
    ,(if ctl (control-version char) char))))

(define key-tree-terminal-code-map
  (keyhandler received-key-default-mapper
              (key #\backspace) (lambda () (list #\rubout))
              ))

(define key-tree-command-map
  (keyhandler-with-fallback
   key-tree-terminal-code-map
   (key 'control #\C) (lambda () (printf "got C-shift-C~n"))
   ))
