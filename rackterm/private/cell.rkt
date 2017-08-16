#lang racket/base

(require "256color.rkt")
(require racket/draw
         racket/class)

(provide (all-defined-out))

(define-struct cell
  ;; character is either a true character or a list of characters
  ;; (to support combining marks).
  (character style)
  #:transparent)

(define (cell-is-combining-mark? cell)
  (let ([c (cell-character cell)])
    (and (char? c)
         (member (char-general-category c) '(mn mc me)))))

(define (append-mark-cell base-cell mark-cell)
  ;; combining marks combine with the character in front of them
  (define base-c (cell-character base-cell))
  (define mark-c (cell-character mark-cell))
  (define new-c (if (list? base-c)
                    (append base-c (list mark-c))
                    (list base-c mark-c)))
  (cell new-c (cell-style base-cell)))

(define-struct style
  ;; colors are symbols 'red, 'blue, etc for 8 color palette
  ;; colors are ints for 256color palette
  ;; colors are color% objects for 24-bit colors
  (fg-color
   bg-color
   bold
   underline
   blink
   strikethrough
   italic
   reverse-video)
  #:transparent)

(define default-style
  (make-style 'default-fg
              'default-bg
              #f
              #f
              #f
              #f
              #f
              #f))

(define blank-cell (make-cell #\space default-style))

(define (style->color% style [fg? #t] [bold-brightens? #t])
  (let* ((fg?? (if (style-reverse-video style)
                   (not fg?)
                   fg?))
         (c (if fg?? (style-fg-color style)
                (style-bg-color style))))
    (cond
      [(is-a? c color%) c]
      [(symbol? c)
       (hash-ref color-symbol-map
                 (if (and bold-brightens? (style-bold style))
                     (string->symbol (string-append "bright-" (symbol->string c)))
                     c))]
      [else (lookup-256color c)])))

(define color-symbol-map
  (let ((c (lambda (r g b) (make-color r g b))))
    (hash 'default-fg (c 200 200 200)
          'bright-default-fg (c 200 200 200)
          'default-bg (c 0 0 0)
          'bright-default-bg (c 0 0 0)
          'black (c 0 0 0)
          'bright-black (c 50 50 50)
          'red (c 255 0 0)
          'bright-red (c 255 50 50)
          'green (c 0 255 0)
          'bright-green (c 50 255 50)
          'brown (c 255 255 0)
          'bright-brown (c 255 255 50)
          'blue (c 0 0 255)
          'bright-blue (c 50 50 255)
          'magenta (c 255 0 255)
          'bright-magenta (c 255 50 255)
          'cyan (c 0 255 255)
          'bright-cyan (c 50 255 255)
          'white (c 200 200 200)
          'bright-white (c 255 255 255))))

