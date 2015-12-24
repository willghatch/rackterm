#lang racket/base
(require racket/draw)
(require racket/list)
(require racket/stream)
(require racket/block)
(require racket/runtime-path)
(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require "pty.rkt")
(require "fun-terminal.rkt")
(require "cell.rkt")
(require "console-code-parse.rkt")

;; This is the main file for the terminal library.  It is to be wrapped by a program
;; to make eg. an xterm or a screen/tmux type emulator, or maybe even a framebuffer
;; terminal!
(provide (all-from-out "cell.rkt")
         init-terminal
         terminal-get-lines
         terminal-title
         send-char-to-terminal-process
         terminal-set-size
         terminal-input-listener
         )


;; TODO:
;; - the 'who' command doesn't show my racket xterms... Also of note: it doesn't show for
;;   st terminals or finalterm either, so maybe it doesn't matter

;; Some notes:
;; - The spec uses 1 based cell addressing -- IE 1,1 is the origin at the top left corner.
;;   I dislike 1 based indexing, so internally I use 0 based indexing.  Things are
;;   translated at the control sequence handling.
;; TODO - use 1-based indexing for rows and columns, since that's what the standard uses.  Translating was a bad idea.

(define-struct terminal
  (fun-terminal-norm
   fun-terminal-alt
   current-alt-screen-state
   process-in
   process-out
   ptm-fd
   current-width
   current-height
   redraw-callback
   current-char-handler
   current-cell-style
   current-scrolling-region ; (cons start-line, end-line)
   current-tab-stops ; sorted list of tab stop indices
   title
   margin-relative-addressing ; do the go-to-row/col commands base on the scrolling region -- DECOM
   subproc-ended-callback
   )
  #:mutable)

(define (init-terminal redraw-callback
                       subproc-ended-callback
                       command-and-args
                       #:term-var [term-var "rackterm"])
  (define (-init-terminal m-in m-out master-fd slave-fd redraw-callback)
    (make-terminal the-empty-fun-terminal
                   the-empty-fun-terminal
                   #f
                   m-in
                   m-out
                   master-fd
                   80
                   24
                   redraw-callback
                   null
                   default-style
                   null
                   '()
                   "rackterm"
                   #f ; margin relative addressing
                   subproc-ended-callback
                   ))
  (define-values (m-in m-out s-in s-out master-fd slave-fd) (openpty))
  (define sub-env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! sub-env #"TERM" (string->bytes/utf-8 term-var))
  (define-values (subproc sub-in sub-out sub-err)
    (parameterize ([current-environment-variables sub-env])
      (apply subprocess (append (list s-out s-in 'stdout
                                      "/usr/bin/env" "racket" "-l" "rackterm/shell-trampoline")
                                command-and-args))))
  (let ((new-term
         (-init-terminal m-in m-out master-fd slave-fd redraw-callback)))
    (terminal-set-default-tab-stops new-term)
    (thread (lambda ()
              (subprocess-wait subproc)
              (subproc-ended-callback)))
    new-term))


(define (terminal-fun-terminal term)
  (if (terminal-current-alt-screen-state term)
      (terminal-fun-terminal-alt term)
      (terminal-fun-terminal-norm term)))
(define (set-terminal-fun-terminal! term ft)
  (if (terminal-current-alt-screen-state term)
      (set-terminal-fun-terminal-alt! term ft)
      (set-terminal-fun-terminal-norm! term ft)))

(define (terminal-mutate terminal fun-terminal-function)
  (set-terminal-fun-terminal! terminal
                              (fun-terminal-function (terminal-fun-terminal
                                                      terminal))))
(define (terminal-scroll-region term n-scrolls)
  (unless (equal? n-scrolls 0)
    (let* ((cur-row (terminal-get-row term))
           (cursor-line-num (terminal-get-row term))
           (region (terminal-current-scrolling-region term))
           (region-start (if (null? region) 0 (car region)))
           (region-end (if (null? region) (sub1 (terminal-current-height term)) (cdr region)))
           (n-pre (cursor-line-num . - . region-start))
           (n-post (region-end . - . cursor-line-num))
           (sign (if (n-scrolls . < . 0) - +))
           (nn-scrolls (sign (min (abs n-scrolls) (+ n-pre n-post 1)))))
      (terminal-mutate term (lambda (ft) (fun-terminal-scroll-region ft n-pre n-post nn-scrolls)))
      (terminal-go-to-row term cur-row))))

(define (terminal-delete-lines-with-scrolling-region term n-deletions)
  (let* ((cur (terminal-get-row term))
         (region (terminal-current-scrolling-region term))
         (region-end (if (null? region)
                         (sub1 (terminal-current-height term))
                         (cdr region)))
         (cur-to-end (- region-end cur)))
    (terminal-mutate term (lambda (ft)
                            (fun-terminal-scroll-region ft 0 cur-to-end n-deletions)))))
(define (terminal-insert-lines-with-scrolling-region term n-inserts)
  (terminal-delete-lines-with-scrolling-region term (- n-inserts)))

(define (terminal-insert-at-cursor term cell)
  (terminal-mutate term (lambda (ft) (fun-terminal-insert-at-cursor ft cell))))
(define (terminal-insert-after-cursor term cell)
  (terminal-insert-at-cursor term cell)
  (terminal-forward-chars term -1))
(define (terminal-delete-forward-at-cursor term n)
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-forward-at-cursor ft n))))
(define (terminal-delete-backwards-at-cursor term)
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-backwards-at-cursor ft))))
(define (terminal-clear-from-start-of-line-to-cursor term)
  (terminal-mutate term (lambda (ft) (fun-terminal-clear-from-start-of-line-to-cursor ft))))
(define (terminal-line-break-at-cursor term)
  (terminal-mutate term (lambda (ft) (fun-terminal-line-break-at-cursor ft))))
(define (terminal-delete-to-end-of-line term)
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-to-end-of-line ft))))
(define (terminal-clear-current-line term)
  (terminal-mutate term (lambda (ft) (fun-terminal-clear-line ft))))
(define (terminal-forward-chars term [n 1])
  (terminal-mutate term (lambda (ft) (fun-terminal-forward-cells ft n))))
(define (terminal-forward-lines term [n 1] #:scrollable? [scrollable? #t])
  (define (-terminal-forward-lines term [n 1])
    (terminal-mutate term (lambda (ft) (fun-terminal-forward-lines ft n))))
  (if (or (not scrollable?) (null? (terminal-current-scrolling-region term)))
      (-terminal-forward-lines term n)
      (let* ((forward? (positive? n))
             (cur (terminal-get-row term))
             (region (terminal-current-scrolling-region term))
             (end (if forward? (cdr region)
                      (car region)))
             (moved (+ cur n))
             (beyond (if ((if forward? > <) moved end)
                         (- moved end)
                         0))
             (to-move (- n beyond))
             ;; If the cursor starts outside of the scrolling region and we try to keep
             ;; moving farther away, to-move and beyond will have opposite signs.
             ;; In this case we should just move, not move and scroll...
             (was-already-beyond (and (not (equal? 0 beyond))
                                      (not (equal? (negative? to-move)
                                                   (negative? beyond))))))
;        (eprintf "forward-n-lines ~a, cur ~a, region ~a, to-move ~a, to-scroll ~a, was-already-beyond ~a~n"
;                n cur region to-move beyond was-already-beyond)
        (if was-already-beyond
            (-terminal-forward-lines term n)
            (begin
              (-terminal-forward-lines term to-move)
              (terminal-scroll-region term beyond))))))

(define (terminal-overwrite term cell)
  ;; This may need looking into when I want to handle re-wrapping on size changes
  (when (equal? (terminal-get-column term) (terminal-current-width term))
    (terminal-forward-lines term 1)
    (terminal-go-to-column term 0))
  (terminal-mutate term (lambda (ft) (fun-terminal-overwrite ft cell))))
(define (terminal-append-line-at-end term)
  (terminal-mutate term (lambda (ft) (fun-terminal-add-blank-line-at-end ft))))
(define (terminal-clear-from-cursor-to-end term)
  (define n-lines (fun-terminal-post-cursor-lines-length (terminal-fun-terminal term)))
  (terminal-delete-to-end-of-line term)
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-post-cursor-lines ft)))
  (for ((i (in-range n-lines)))
    (terminal-append-line-at-end term)))
(define (terminal-insert-blank term [n 1] [after-cursor? #f])
  (let ((insert (if after-cursor?
                    terminal-insert-after-cursor
                    terminal-insert-at-cursor)))
    (for ((i (in-range n)))
      (insert term (terminal-make-cell term #\space)))))
(define (terminal-clear-from-start-to-cursor term)
  (define rows (terminal-get-row term))
  (define cols (terminal-get-column term))
  (terminal-clear-current-line term)
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-n-pre-cursor-lines ft rows)))
  (for ((i (in-range rows)))
    (terminal-line-break-at-cursor term))
  (terminal-insert-blank term cols))

(define (terminal-set-scrolling-region term start+1 end+1)
  (define start (sub1 start+1))
  (define end (if (equal? end+1 'end)
                  (sub1 (terminal-current-height term))
                  (sub1 end+1)))
  (if (and (equal? start 0)
           (equal? end (sub1 (terminal-current-height term))))
      (set-terminal-current-scrolling-region! term null)
      (set-terminal-current-scrolling-region! term (cons start end))))


(define (terminal-set-size term width height)
  (define n-rows (fun-terminal-get-num-rows (terminal-fun-terminal term)))
  (define row-diff (- height n-rows))
  (when (> row-diff 0)
    (for ((i (in-range row-diff)))
      (terminal-append-line-at-end term)))

  (set-terminal-current-width! term width)
  (set-terminal-current-height! term height)
  (set-pty-size (terminal-ptm-fd term) (new-winsize width height)))

(define (terminal-set-tab-stop term)
  (let ((stops (terminal-current-tab-stops term))
        (col (terminal-get-column term)))
    (unless (member col stops)
      (set-terminal-current-tab-stops! term (sort (cons col stops) <)))))
(define (terminal-remove-tab-stop term)
  (set-terminal-current-tab-stops! term (remove (terminal-get-column term)
                                                (terminal-current-tab-stops term))))
(define (terminal-remove-all-tab-stops term)
  (set-terminal-current-tab-stops! term '()))
(define (terminal-set-default-tab-stops term)
  (set-terminal-current-tab-stops! term (map (lambda (x) (* 8 x))
                                             (stream->list (in-range 50)))))

(define (terminal-go-to-next-tab-stop term)
  (define (find-tab tabs column)
    (cond [(null? tabs)
           (sub1 (terminal-current-width term))]
          [(<= (car tabs) column)
           (find-tab (cdr tabs) column)]
          [else (car tabs)]))
  (let* ((cur-col (terminal-get-column term))
         (next-tab (find-tab (terminal-current-tab-stops term) cur-col))
         (diff (- next-tab cur-col)))
    (terminal-forward-chars term diff)))

(define (send-char-to-terminal-process term char)
  (write-char char (terminal-process-out term))
  (flush-output (terminal-process-out term)))

(define (terminal-get-lines term)
  (fun-terminal->lines-from-end (terminal-fun-terminal term) #t))

(define (terminal-make-cell term char)
  (make-cell char (terminal-current-cell-style term)))

(define (terminal-insert-character term char)
  (terminal-insert-at-cursor term (terminal-make-cell term char)))

(define (terminal-get-column term)
  (fun-terminal-get-column (terminal-fun-terminal term)))
(define (terminal-get-row term)
  (let* ((from-end (fun-terminal-get-rows-from-end (terminal-fun-terminal term)))
         (size (terminal-current-height term)))
    (- size from-end 1)))

(define (terminal-go-to-column term column)
  (let* ((cur-column (terminal-get-column term))
        (diff (column . - . cur-column)))
    (terminal-forward-chars term diff)))
(define (terminal-go-to-row term row)
  (let* ((cur-row (terminal-get-row term))
         (relative-cur-row (if (and (terminal-margin-relative-addressing term)
                                    (terminal-current-scrolling-region term))
                               (- cur-row (car (terminal-current-scrolling-region term)))
                               cur-row))
         (diff (row . - . relative-cur-row)))
    (terminal-forward-lines term diff #:scrollable? #f)))
(define (terminal-go-to-row-column term row [column 0])
  (terminal-go-to-row term row)
  (terminal-go-to-column term column))


(define (terminal-overwrite-character term char)
  ;(eprintf "writing character ~s~n" char)
  (terminal-overwrite term (terminal-make-cell term char)))

(define (terminal-input-listener term)
  (define ns (mk-terminal-namespace term))
  (define (read-char-from-terminal-process term)
    (read-char (terminal-process-in term)))
  (lambda ()
    (define (listener p-state)
      (let ((char (read-char-from-terminal-process term)))
        (if (not (eof-object? char))
            (block
              (define-values (n-state output) (parse-char char #:parser-state p-state))
              ;(terminal-interp term output)
              (when (not (null? output))
                (with-handlers ([(λ _ #t)
                                 (λ e (eprintf "Caught exception during terminal eval of ~v:~n~a~n" output e))])
                  (eval output ns))
                ((terminal-redraw-callback term)))
              (listener n-state))
            (void))))
    (sleep 0)
    (listener #f)))


(define (set-term-color! term fg? . color-args)
  (let* ([color (cond [(equal? 3 (length color-args)) (apply make-color color-args)]
                      [else (car color-args)])]
         [cur-style (terminal-current-cell-style term)]
         [fg (if fg? color (style-fg-color cur-style))]
         [bg (if fg? (style-bg-color cur-style) color)])
    (set-terminal-current-cell-style!
     term
     (struct-copy style cur-style [fg-color fg] [bg-color bg]))))

(define-syntax (def-set-style-bool stx)
  (syntax-case stx ()
    [(_ attr)
     (with-syntax ([fname (format-id #'attr "set-style-~a!" #'attr)])
       #'(define (fname term set?)
           (set-terminal-current-cell-style!
            term (struct-copy style
                              (terminal-current-cell-style term)
                              [attr set?]))))]))

(def-set-style-bool bold)
(def-set-style-bool italic)
(def-set-style-bool underline)
(def-set-style-bool blink)
(def-set-style-bool reverse-video)


(define (terminal-forward-lines-column-0 term n)
  (terminal-go-to-column term 0)
  (terminal-forward-lines term n))
(define (terminal-do-esc-M term)
  (let* ((region (terminal-current-scrolling-region term))
         (beginning (if (pair? region) (car region) 0)))
    (if (equal? beginning (terminal-get-row term))
        (terminal-scroll-region term -1)
        (terminal-forward-lines term -1))))
(define (set-style-default! term)
  (set-terminal-current-cell-style! term default-style))
(define (set-style-fg-color! term . args)
  (apply set-term-color! `(,term #t ,@args)))
(define (set-style-bg-color! term . args)
  (apply set-term-color! `(,term #f ,@args)))
(define (terminal-clear term)
  (terminal-clear-from-start-to-cursor term)
  (terminal-clear-from-cursor-to-end term))
(define (terminal-replace-chars-with-space term n)
  (terminal-delete-forward-at-cursor term n)
  (terminal-insert-blank term n))

;;; Some convenience functions for writing s-expressions
(define (terminal-write-string term str)
  (for ((c str))
    (terminal-overwrite-character term c)))


(define (mk-terminal-namespace term)
  #|
  TODO
  What I really want is to be able to put ALL of racket/base with require, or
  all of R5RS or some such, but without network access, file access, FFI access...
  Basically I want it to be all of racket or scheme, but with safety.

  Also, I would probably like to enable both r5rs AND racket separately --
  perhaps default to one for normal forms that I get from standard terminal
  codes, but have two s-expression parsing codes, one for racket and one for scheme.

  At any rate, I need at least a way to let this reset so that one application can
  define things without worrying about what another has already defined...

  |#
  (define ns (make-base-empty-namespace))
  (parameterize [(current-namespace ns)]
    (namespace-require '(only racket/base
                              #%app
                              #%datum
                              quote
                              quasiquote
                              unquote
                              unquote-splicing
                              begin
                              define
                              lambda
                              λ
                              let
                              apply
                              )))
  (define (tfun f)
    (λ args (apply f (cons term args))))
  (define (def sym val)
    (namespace-set-variable-value! sym val #t ns))
  (define-syntax-rule (tdef name func)
    (def name (tfun func)))

  (tdef 'terminal-write-char terminal-overwrite-character)
  (tdef 'terminal-write-string terminal-write-string)
  (tdef 'terminal-forward-chars terminal-forward-chars)
  (tdef 'terminal-crlf (λ (term [n 1])
                         (terminal-forward-lines term n)
                         (terminal-go-to-column term 0)))
  (tdef 'terminal-forward-lines terminal-forward-lines)
  (tdef 'terminal-forward-lines-column-0 terminal-forward-lines-column-0)
  (tdef 'terminal-go-to-row terminal-go-to-row)
  (tdef 'terminal-go-to-column terminal-go-to-column)
  (tdef 'terminal-go-to-row-column terminal-go-to-row-column)
  (tdef 'terminal-do-esc-M terminal-do-esc-M)
  (tdef 'terminal-go-to-next-tab-stop terminal-go-to-next-tab-stop)
  (tdef 'terminal-set-tab-stop terminal-set-tab-stop)
  (tdef 'terminal-set-title! set-terminal-title!)
  (tdef 'set-terminal-margin-relative-addressing! set-terminal-margin-relative-addressing!)
  (tdef 'set-terminal-current-alt-screen-state! set-terminal-current-alt-screen-state!)
  (tdef 'set-style-default! set-style-default!)
  (tdef 'set-style-fg-color! set-style-fg-color!)
  (tdef 'set-style-bg-color! set-style-bg-color!)
  (tdef 'set-style-bold! set-style-bold!)
  (tdef 'set-style-italic! set-style-italic!)
  (tdef 'set-style-underline! set-style-underline!)
  (tdef 'set-style-blink! set-style-blink!)
  (tdef 'set-style-reverse-video! set-style-reverse-video!)
  (tdef 'insert-blanks terminal-insert-blank)
  (tdef 'terminal-clear terminal-clear)
  (tdef 'terminal-clear-from-start-to-cursor terminal-clear-from-start-to-cursor)
  (tdef 'terminal-clear-from-cursor-to-end terminal-clear-from-cursor-to-end)
  (tdef 'terminal-clear-current-line terminal-clear-current-line)
  (tdef 'terminal-clear-from-start-of-line-to-cursor terminal-clear-from-start-of-line-to-cursor)
  (tdef 'terminal-delete-to-end-of-line terminal-delete-to-end-of-line)
  (tdef 'terminal-insert-lines-with-scrolling-region terminal-insert-lines-with-scrolling-region)
  (tdef 'terminal-delete-lines-with-scrolling-region terminal-delete-lines-with-scrolling-region)
  (tdef 'terminal-delete-forward-at-cursor terminal-delete-forward-at-cursor)
  (tdef 'terminal-scroll-region terminal-scroll-region)
  (tdef 'terminal-replace-chars-with-space terminal-replace-chars-with-space)
  (tdef 'terminal-remove-all-tab-stops terminal-remove-all-tab-stops)
  (tdef 'terminal-remove-tab-stop terminal-remove-tab-stop)
  (tdef 'terminal-set-scrolling-region terminal-set-scrolling-region)

  (tdef 'unknown-control-character (λ (t . r) (eprintf "unknown control character: ~a~n" r)))
  (tdef 'unknown-escape-character (λ (t . r) (eprintf "unknown escape character: ~a~n" r)))
  (tdef 'ignored-escape-sequence (λ (t . r) (eprintf "ignored escape sequence: ~a~n" r)))
  (tdef 'unknown-csi-terminator (λ (t . r) (eprintf "unknown csi terminator: ~a~n" r)))
  (tdef 'unknown-mode-set (λ (t . r) (eprintf "unknown mode set: ~a~n" r)))
  (tdef 'bell (λ (t . r) (eprintf "Bell!~n")))

  ns)
