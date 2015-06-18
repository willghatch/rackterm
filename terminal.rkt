#lang racket/base
(require racket/draw)
(require racket/list)
(require racket/stream)
(require "pty.rkt")
(require "fun-terminal.rkt")
(require "cell.rkt")

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
;        (printf "forward-n-lines ~a, cur ~a, region ~a, to-move ~a, to-scroll ~a, was-already-beyond ~a~n"
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

(define (terminal-set-scrolling-region term start end)
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


(define (terminal-overwrite-character term char)
  ;(printf "writing character ~s~n" char)
  (terminal-overwrite term (terminal-make-cell term char)))

(define (terminal-handle-character term char)
  ;(printf "handling: ~s~n" char)
  (define handler (terminal-current-char-handler term))
  (with-handlers ([(λ (exn) #t) (λ (exn) ((error-display-handler) (exn-message exn) exn))])
    (cond
      [(not (null? handler)) (handler term char)]
      [((char->integer char) . < . 32)
       (handle-ascii-controls term char)]
      [else
       (terminal-overwrite-character term char)]))
  ((terminal-redraw-callback term)))

(define (terminal-input-listener term)
  (define (read-char-from-terminal-process term)
    (read-char (terminal-process-in term)))
  (lambda ()
    (define (listener)
      (let ((char (read-char-from-terminal-process term)))
        (if (not (eof-object? char))
            (begin (terminal-handle-character term char)
                   (listener))
            (void))))
    (sleep 0)
    (listener)))

(define (handle-ascii-controls term char)
  (case char
    [(#\u07) null] ;; BEEP!
    [(#\u08) (terminal-forward-chars term -1)] ;; backspace
    [(#\u09) (terminal-go-to-next-tab-stop term)]
    [(#\newline #\u0B #\u0C) (terminal-forward-lines term)]
    [(#\return) (terminal-go-to-column term 0)] ;; carriage return...
    [(#\u0E) null] ;; activate G1 character set
    [(#\u0F) null] ;; activate G0 character set
    [(#\u1B) (set-terminal-current-char-handler! term escape-handler)] ;; start escape sequence
    [(#\u9B) (set-terminal-current-char-handler! term new-csi-handler)]
    [else (printf "ignored control char: ~s~n" char)]))

(define (escape-handler term char)
  ;; IE handling after receiving ESC character
  (set-terminal-current-char-handler! term null)
  (case char
    [(#\D) (terminal-forward-lines term)]
    [(#\H) (terminal-set-tab-stop term)]
    ;; M should scroll up one line.  If at the top, it should remove the bottom line and insert one
    [(#\M) (let* ((region (terminal-current-scrolling-region term))
                  (beginning (if (pair? region) (car region) 0)))
             (if (equal? beginning (terminal-get-row term))
                 (terminal-scroll-region term -1)
                 (terminal-forward-lines term -1)))]
    [(#\[) (set-terminal-current-char-handler! term new-csi-handler)]
    [(#\]) (set-terminal-current-char-handler! term new-osc-handler)]

    ;; for setting 7 or 8 bit controls, ascii conformance...
    [(#\space) (set-terminal-current-char-handler! term ignore-next-char)]

    ;;; The rest are less important
    [(#\#) (set-terminal-current-char-handler! term ignore-next-char)]
    [(#\%) (set-terminal-current-char-handler! term ignore-next-char)]
    [(#\+) (set-terminal-current-char-handler! term ignore-next-char)]
    [(#\-) (set-terminal-current-char-handler! term ignore-next-char)]
    [(#\*) (set-terminal-current-char-handler! term ignore-next-char)]
    [(#\/) (set-terminal-current-char-handler! term ignore-next-char)]
    [(#\.) (set-terminal-current-char-handler! term ignore-next-char)]


    ;; these paren ones have something to do with setting character sets
    [(#\() (set-terminal-current-char-handler! term ignore-next-char)]
    [(#\)) (set-terminal-current-char-handler! term ignore-next-char)]
    [else (printf "ignored escaped character: ~s~n" char)]))

(define (make-osc-handler numeric-arg)
  ;; osc sequences are "ESC ] number ; <usually text> <ST - string terminator>
  (lambda (term char)
    (case char
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (set-terminal-current-char-handler!
        term
        (make-osc-handler (+ (* 10 numeric-arg)
                             (read (open-input-string (string char))))))]
      ;; the only other character should be the semicolon
      [else (set-terminal-current-char-handler! term
                                                (make-osc-text-handler numeric-arg "" #f))])))
(define new-osc-handler (make-osc-handler 0))
(define (make-osc-text-handler numeric-arg text previous-char)
  (lambda (term char)
    ;; the string terminator is ESC-\
    ;; but xterm seems to support just #\u07 (ascii BELL)
    (cond ((and (equal? char #\\)
                (equal? previous-char #\u1B))
           (osc-handler-finish term numeric-arg text))
          ((equal? char #\u07)
           (osc-handler-finish term numeric-arg (string-append text (string previous-char))))
          (else (set-terminal-current-char-handler!
                term
                (let ((new-text (if previous-char
                                    (string-append text (string previous-char))
                                    "")))
                  (make-osc-text-handler numeric-arg
                                         new-text
                                         char)))))))

(define (osc-handler-finish term numeric-arg text)
  (set-terminal-current-char-handler! term null)
  (case numeric-arg
    [(0 1 2) (set-terminal-title! term text)]
    [(3) (void)] ; this should set X properties.
    [else (void)])) ; aaaand some other stuff.

(define (make-ignore-next-n-characters-handler n)
  (lambda (term char)
    (if (n . < . 2)
        (set-terminal-current-char-handler! term null)
        (set-terminal-current-char-handler!
         term
         (make-ignore-next-n-characters-handler (sub1 n))))))
(define ignore-next-char (make-ignore-next-n-characters-handler 1))

(define (make-csi-handler completed-params current-param leading-question?)
  ;; CSI sequences start with 'ESC [', then possibly a question mark (which seems
  ;; to only matter for setting/resetting modes with h/l, in which case it means
  ;; "private mode"), then decimal number arguments separated by semicolons,
  ;; until a final character that determines the function.
  (lambda (term char)
    (set-terminal-current-char-handler! term null)
    (case char
      [(#\;) (set-terminal-current-char-handler! term
                                                 (make-csi-handler
                                                  (append completed-params (list current-param))
                                                  0
                                                  leading-question?))]
      [(#\?) (if (and (null? completed-params) (equal? current-param 0))
                 (set-terminal-current-char-handler! term
                                                     (make-csi-handler
                                                      completed-params
                                                      current-param
                                                      #t))
                 null)] ; I guess a question mark somewhere else just kills it...?
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (set-terminal-current-char-handler! term (make-csi-handler
                                                 completed-params
                                                 (+ (* 10 current-param)
                                                    (read (open-input-string (string char))))
                                                 leading-question?))]
      [else
       (let ((end-handler (hash-ref csi-table char (lambda ()
                                                     (lambda (term char params lq?)
                                                       (printf "ignored CSI terminator: ~a with params: ~a~n" char params)))))
             (final-params (append completed-params (list current-param))))

         (printf "handling CSI sequence ending in ~a.  Params: ~a lq?: ~a~n" char final-params leading-question?)
         (end-handler term char final-params leading-question?))])))

(define new-csi-handler (make-csi-handler '() 0 #f))

(define (car-defaulted l default)
  (if (or (null? l)
          (equal? (car l) 0))
      default
      (car l)))
(define (cadr-defaulted l default)
  (cond
    [(< (length l) 2) default]
    [(equal? (cadr l) 0) default]
    [else (cadr l)]))

(define (handle-set-mode term char params private?)
  ;; reset if l, set if h
  (define on? (equal? char #\h))
  (define (ignore)
    (printf "ignoring mode set - on? ~a, private? ~a, params ~a~n"
            on? private? params))
  (define setting (car-defaulted params 0))
  (if private?
      (case setting
        [(6) (set-terminal-margin-relative-addressing! on?)]
        [(1049) (begin
                  (set-terminal-current-alt-screen-state! term on?)
                  (set-terminal-fun-terminal-alt! term the-empty-fun-terminal))]
        [else (ignore)])
      (case setting
        [else (ignore)]))
  ;; recurse to handle any more settings, because they can be set in groups
  (if (null? (cdr params))
      (void)
      (handle-set-mode term char (cdr params) private?)))

(define (color-csi-handler term char params lq?)
  ;; TODO - check all the ones listed on the wikipedia page for ansi escape codes...
  ;; there are a lot of obscure ones
  ;; 24 bit color = CSI-38;2;r;g;bm for fg and 48 instead of 38 for bg
  ;; for 256 color pallete, CSI-38;5;colorm
  (set-terminal-current-char-handler! term null)
  (define old-style (terminal-current-cell-style term))
  (define (set-style-and-handle style)
    (set-terminal-current-cell-style! term style)
    (color-csi-handler term char (cdr params) lq?))
  (define (fg color)
    (set-style-and-handle (struct-copy
                           style
                           (terminal-current-cell-style term)
                           [fg-color color])))
  (define (bg color)
    (set-style-and-handle (struct-copy
                           style
                           (terminal-current-cell-style term)
                           [bg-color color])))
  (if (null? params)
      'done
      (case (car params)
        [(0) (set-style-and-handle default-style)]
        [(1) (set-style-and-handle (struct-copy style old-style
                                                [bold #t]))]
        ;[(2) null]
        [(3) (set-style-and-handle (struct-copy style old-style
                                                [italic #t]))]
        [(4) (set-style-and-handle (struct-copy style old-style
                                                [underline #t]))]
        [(5) (set-style-and-handle (struct-copy style old-style
                                                [blink #t]))]
        [(7) (set-style-and-handle (struct-copy style old-style
                                                [reverse-video #t]))]
        ;[(10) null]
        ;[(11) null]
        ;[(12) null]
        ;[(21) null]
        [(22) (set-style-and-handle (struct-copy style old-style
                                                [bold #f]))]
        [(23) (set-style-and-handle (struct-copy style old-style
                                                [italic #f]))]
        [(24) (set-style-and-handle (struct-copy style old-style
                                                [underline #f]))]
        [(25) (set-style-and-handle (struct-copy style old-style
                                                [blink #f]))]
        [(27) (set-style-and-handle (struct-copy style old-style
                                                [reverse-video #f]))]
        [(30) (fg 'black)]
        [(31) (fg 'red)]
        [(32) (fg 'green)]
        [(33) (fg 'brown)]
        [(34) (fg 'blue)]
        [(35) (fg 'magenta)]
        [(36) (fg 'cyan)]
        [(37) (fg 'white)]
        [(38) (extended-color-handler term char (cdr params) #t)]
        [(39) (fg 'default-fg)]
        [(40) (bg 'black)]
        [(41) (bg 'red)]
        [(42) (bg 'green)]
        [(43) (bg 'brown)]
        [(44) (bg 'blue)]
        [(45) (bg 'magenta)]
        [(46) (bg 'cyan)]
        [(47) (bg 'white)]
        [(48) (extended-color-handler term char (cdr params) #f)]
        [(49) (bg 'default-bg)]
        [else (color-csi-handler term char (cdr params) lq?)])))

(define (extended-color-handler term char params fg?)
  (define (bg color)
    (set-terminal-current-cell-style!
     term
     (struct-copy style (terminal-current-cell-style term)
                  [bg-color color])))
  (define (fg color)
    (set-terminal-current-cell-style!
     term
     (struct-copy style (terminal-current-cell-style term)
                  [fg-color color])))
  (define setc (if fg? fg bg))
  (cond
    [(null? params) 'done]
    [(equal? (car params) 2)
     (if (< (length params) 4)
         null
         (begin
           (setc (make-color (second params) (third params) (fourth params)))
           (color-csi-handler term char (list-tail params 4) #f)))]
    [(equal? (car params) 5)
     (if (< (length params) 2)
         null
         (begin
           (setc (second params))
           (color-csi-handler term char (list-tail params 2) #f)))]
    [else (color-csi-handler term char (cdr params) #f)]))

(define csi-table
  (hash
   ;; insert blanks
   #\@ (lambda (term char params lq?)
         (let ((n (car-defaulted params 1)))
           (terminal-insert-blank term n #t)))

   ;; forward lines
   #\A (lambda (term char params lq?)
         (terminal-forward-lines term (- (car-defaulted params 1))))
   #\B (lambda (term char params lq?)
         (terminal-forward-lines term (car-defaulted params 1)))

   ;; forward chars
   #\C (lambda (term char params lq?)
         (terminal-forward-chars term (car-defaulted params 1)))
   #\D (lambda (term char params lq?)
         (terminal-forward-chars term (- (car-defaulted params 1))))

   ;; forward lines to column 0
   #\E (lambda (term char params lq?)
         (terminal-go-to-column term 0)
         (terminal-forward-lines term (car-defaulted params 1)))
   #\F (lambda (term char params lq?)
         (terminal-go-to-column term 0)
         (terminal-forward-lines term (- (car-defaulted params 1))))

   ;; go to address directly
   #\G (lambda (term char params lq?)
         (terminal-go-to-column term (sub1 (car-defaulted params 1))))
   #\H (lambda (term char params lq?)
         (terminal-go-to-row term (sub1 (car-defaulted params 1)))
         (terminal-go-to-column term (sub1 (cadr-defaulted params 1))))

   ;; clear screen
   #\J (lambda (term char params lq?)
         (let ((n (car params)))
           (case n
             ;; 3 is supposed to clear including the scrollback buffer in the Linux
             ;;   terminal, but I don't think I care for that feature.
             [(2) (begin
                    (terminal-clear-from-start-to-cursor term)
                    (terminal-clear-from-cursor-to-end term))]
             [(1) (terminal-clear-from-start-to-cursor term)]
             ;; 0
             [else (terminal-clear-from-cursor-to-end term)])))

   ;; clear line
   #\K (lambda (term char params lq?)
         (let ((n (car params)))
           (case n
             [(2) (terminal-clear-current-line term)]
             [(1) (terminal-clear-from-start-of-line-to-cursor)]
             ;; 0
             [else (terminal-delete-to-end-of-line term)])))

   ;; L - insert n blank lines
   #\L (lambda (term char params lq?)
         (terminal-insert-lines-with-scrolling-region term (car-defaulted params 1)))
   ;; M - delete n lines
   #\M (lambda (term char params lq?)
         (terminal-delete-lines-with-scrolling-region term (car-defaulted params 1)))
   ;; P - delete n characters on current line -- meaning characters shift left
   #\P (lambda (term char params lq?)
         (terminal-delete-forward-at-cursor term (car-defaulted params 1)))
   ;; S scroll up n lines
   #\S (lambda (term char params lq?)
         (terminal-scroll-region term (car-defaulted params 1)))
   ;; T scroll down n lines
   #\T (lambda (term char params lq?)
         (terminal-scroll-region term (- (car-defaulted params 1))))
   ;; X - erase n characters on current line -- meaning characters are replaced with spaces
   #\X (lambda (term char params lq?)
         (let ((n (car-defaulted params 1)))
           (terminal-delete-forward-at-cursor term n)
           (terminal-insert-blank term n)))

   ;; Half of these are duplicates. Stupid.
   ;; Forward chars
   #\a (lambda (term char params lq?)
         (terminal-forward-chars term (car-defaulted params 1)))
   ;; b... I don't see a spec for it
   ;; c -- some sort of terminal identification...

   #\d (lambda (term char params lq?)
         (terminal-go-to-row term (sub1 (car-defaulted params 1))))
   #\e (lambda (term char params lq?)
         (terminal-forward-lines term (car-defaulted params 1)))
   #\f (lambda (term char params lq?)
         (terminal-go-to-row term (sub1 (car-defaulted params 1)))
         (terminal-go-to-column term (sub1 (cadr-defaulted params 1))))

   ;; g - 0 - clear tab stop at current position
   ;;     3 - delete all tab stops
   #\g (lambda (term char params lq?)
         (let ((arg (car-defaulted params 1)))
           (if (equal? arg 3)
               (terminal-remove-all-tab-stops term)
               (terminal-remove-tab-stop term))))

   ;; h - set mode
   ;; l - reset mode
   #\h handle-set-mode
   #\l handle-set-mode


   #\m color-csi-handler

   ;; n - status report
   ;; q - keyboard LEDs

   ;; set scrolling region
   #\r (lambda (term char params lq?)
         (let ((start (sub1 (car-defaulted params 1)))
               (end (sub1 (cadr-defaulted params (terminal-current-height term)))))
           (terminal-set-scrolling-region term start end)))

   ;; s - save cursor location
   ;; u - restore cursor location


   #\` (lambda (term char params lq?)
         (terminal-go-to-column term (sub1 (car-defaulted params 1))))
   ))

