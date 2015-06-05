#lang racket/base
(require racket/system) ; for process/ports
(require racket/draw)
(require racket/list)
(require "pty.rkt")
(require "fun-terminal.rkt")
(require "256color.rkt")


(provide (all-defined-out)
         (struct-out cell))


;; TODO:
;; - the 'who' command doesn't show my racket xterms... Also of note: it doesn't show for
;;   st terminals either, which is where I looked at how to do the pty stuff.
;; - /bin/sh enters a space once it reaches the last character of terminal width, then a return character.
;;   I believe the default behaviour would be to enter the space on the next line down, then
;;   the carriage return would go to the start of the line...
;;   I can either split the line or do a bunch of handling to make carriage returns only go back
;;   to the line start modulo official line width.
;;   There is a switch to toggle auto-wrapping on end of line or not in the "h" CSI toggles.
;; - I need to report my actual terminal size...

;; Some notes:
;; - The spec uses 1 based cell addressing -- IE 1,1 is the origin at the top left corner.
;;   I dislike 1 based indexing, so internally I use 0 based indexing.  Things are
;;   translated at the control sequence handling.

(define-struct terminal
  (fun-terminal
   process-in
   process-out
   ptm-fd
   pts-fd
   current-width
   current-height
   redraw-callback
   current-char-handler
   current-fg-color
   current-bg-color
   current-cell-attrs
   current-scrolling-region ; (cons start-line, end-line)
   current-tab-stops ; sorted list of tab stop indices
   title
   )
  #:mutable)

(define (terminal-mutate terminal fun-terminal-function)
  (set-terminal-fun-terminal! terminal
                              (fun-terminal-function (terminal-fun-terminal
                                                      terminal))))
(define (terminal-scroll-region term n-scrolls)
  (unless (equal? n-scrolls 0)
    (printf "scrolling... ~a lines~n" n-scrolls)
    (let* ((cursor-line-num (terminal-get-row term))
           (region (terminal-current-scrolling-region term))
           (region-start (car region))
           (region-end (cdr region))
           (n-pre (cursor-line-num . - . region-start))
           (n-post (region-end . - . cursor-line-num)))
      (terminal-mutate term (lambda (ft) (fun-terminal-scroll-region ft n-pre n-post n-scrolls)))
      (terminal-mutate term (lambda (ft) (fun-terminal-forward-lines ft n-scrolls))))))

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
(define (-terminal-forward-lines term [n 1])
  (terminal-mutate term (lambda (ft) (fun-terminal-forward-lines ft n))))
(define (terminal-forward-lines term [n 1])
  (if (null? (terminal-current-scrolling-region term))
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
             (to-move (- n beyond)))
        (-terminal-forward-lines term to-move)
        (terminal-scroll-region term beyond))))

(define (terminal-overwrite term cell)
  (terminal-mutate term (lambda (ft) (fun-terminal-overwrite ft cell))))
(define (terminal-append-line-at-end term)
  (terminal-mutate term (lambda (ft) (fun-terminal-add-blank-line-at-end ft))))
(define (terminal-clear-from-cursor-to-end term)
  (define n-lines (fun-terminal-length-lines-after-cursor (terminal-fun-terminal term)))
  (terminal-delete-to-end-of-line term)
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-lines-after-cursor ft)))
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
  (terminal-mutate term (lambda (ft) (fun-terminal-delete-n-lines-before-cursor ft rows)))
  (for ((i (in-range rows)))
    (terminal-line-break-at-cursor term))
  (terminal-insert-blank term cols))

(define (terminal-set-scrolling-region term start end)
  (if (and (equal? start 0)
           (equal? end (sub1 (terminal-current-height term))))
      (set-terminal-current-scrolling-region! term null)
      (set-terminal-current-scrolling-region! term (cons start end))))

(define default-fg-color "white")
(define default-bg-color "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are experiments on how I should best open the subprocess...

(define (init-terminal4 redraw-callback command . command-args)
  (define-values (m-in m-out s-in s-out master-fd slave-fd) (my-openpty))
    (define-values (subproc sub-in sub-out sub-err)
      (apply subprocess (append (list s-out s-in 'stdout
                                      "/usr/bin/racket" "/home/wgh/mk/rackterm/set-tty.rkt"
                                      (number->string slave-fd)
                                      command)
                                command-args)))
    (make-terminal (make-empty-fun-terminal)
                   m-in
                   m-out
                   master-fd
                   slave-fd
                   80
                   24
                   redraw-callback
                   null
                   default-fg-color
                   default-bg-color
                   '()
                   null
                   '()
                   "rackterm"
                   ))
(define (init-terminal3 redraw-callback command . command-args)
  (define-values (m-in m-out s-in s-out master-fd slave-fd) (my-openpty))
  (apply subproc-with-new-controlling-tty (append (list slave-fd command) command-args))
  (make-terminal (make-empty-fun-terminal)
                 m-in
                 m-out
                 master-fd
                 slave-fd
                 80
                 24
                 redraw-callback
                 null
                 default-fg-color
                 default-bg-color
                 '()
                 null
                 '()
                 "rackterm"
                 ))
(define (init-terminal2 redraw-callback command . command-args)
  (define-values (m-in m-out s-in s-out master-fd slave-fd) (my-openpty))
  (parameterize ([subprocess-group-enabled #t])
    (define-values (subproc sub-in sub-out sub-err)
      (apply subprocess (append (list s-out s-in 'stdout "/usr/bin/setsid") (cons command command-args))))
    (make-terminal (make-empty-fun-terminal)
                   m-in
                   m-out
                   master-fd
                   slave-fd
                   80
                   24
                   redraw-callback
                   null
                   default-fg-color
                   default-bg-color
                   '()
                   null
                   '()
                   "rackterm"
                   )))
(define (init-terminal command redraw-callback)
  (define-values (m-in m-out s-in s-out master-fd slave-fd) (my-openpty))
  (let ((proc (process/ports s-out s-in 'stdout command)))
    (make-terminal (make-empty-fun-terminal)
                   m-in
                   m-out
                   master-fd
                   slave-fd
                   80
                   24
                   redraw-callback
                   null
                   default-fg-color
                   default-bg-color
                   '()
                   null
                   '()
                   "rackterm"
                   )))

(define (terminal-set-size term width height)
  (printf "setting terminal size: ~a ~a~n" width height)
  (define n-rows (fun-terminal-get-num-rows (terminal-fun-terminal term)))
  (define row-diff (- height n-rows))
  (when (> row-diff 0)
    (for ((i (in-range row-diff)))
      (terminal-append-line-at-end term)))

  (set-terminal-current-width! term width)
  (set-terminal-current-height! term height)
  (set-pty-size (terminal-pts-fd term) (new-winsize width height)))

(define (terminal-set-tab-stop term index)
  (let ((stops (terminal-current-tab-stops term)))
    (unless (member index stops)
      (set-terminal-current-tab-stops! term (sort (cons index stops) <)))))
(define (terminal-remove-tab-stop term index)
  (set-terminal-current-tab-stops! term (remove index (terminal-current-tab-stops term))))
(define (terminal-remove-all-tab-stops term)
  (set-terminal-current-tab-stops! term '()))

(define (send-char-to-terminal-process term char)
  (write-char char (terminal-process-out term))
  (flush-output (terminal-process-out term)))

(define (terminal-get-lines term)
  (fun-terminal->lines-from-end (terminal-fun-terminal term) #t))

(define (terminal-make-cell term char)
  (make-cell char
             (terminal-current-fg-color term)
             (terminal-current-bg-color term)
             (terminal-current-cell-attrs term)))

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
        (diff (row . - . cur-row)))
    (terminal-forward-lines term diff)))


(define (terminal-overwrite-character term char)
  (terminal-overwrite term (terminal-make-cell term char)))

(define (terminal-handle-character term char)
  ;(printf "handling: ~s~n" char)
  (define handler (terminal-current-char-handler term))
  (cond
    [(not (null? handler)) (handler term char)]
    [((char->integer char) . < . 32)
     (handle-ascii-controls term char)]
    [else
     (terminal-overwrite-character term char)])
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
            null)))
    (sleep 0)
    (listener)))

(define (handle-ascii-controls term char)
  (case char
    [(#\u07) null] ;; BEEP!
    [(#\u08) (terminal-forward-chars term -1)] ;; backspace
    [(#\u09) null] ;; tab
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
                                                       (printf "ignored CSI terminator: ~a with params: ~a~n" char params))))))
         (end-handler term char (append completed-params (list current-param)) leading-question?))])))

(define new-csi-handler (make-csi-handler '() 0 #f))

(define (car-defaulted l default)
  (let ((orig (car l)))
    (if (equal? 0 orig)
        default
        orig)))
(define (cadr-defaulted l default)
  (cond
    [(< (length l) 2) default]
    [(equal? (cadr l) 0) default]
    [else (cadr l)]))

(define (color-csi-handler term char params lq?)
  ;; TODO - check all the ones listed on the wikipedia page for ansi escape codes...
  ;; there are a lot of obscure ones
  ;; 24 bit color = CSI-38;2;r;g;bm for fg and 48 instead of 38 for bg
  ;; for 256 color pallete, CSI-38;5;colorm
  (set-terminal-current-char-handler! term null)
  (define (fg color) (set-terminal-current-fg-color! term color))
  (define (bg color) (set-terminal-current-bg-color! term color))
  (if (null? params)
      'done
      (begin
        (case (car params)
          [(0) (begin (fg default-fg-color)
                      (bg default-bg-color)
                      (set-terminal-current-cell-attrs! term '()))]
          [(1) null] ; bold
          [(2) null]
          [(4) null]
          [(5) null]
          [(7) null]
          [(10) null]
          [(11) null]
          [(12) null]
          [(21) null]
          [(22) null]
          [(24) null]
          [(25) null]
          [(27) null]
          [(30) (fg "black")]
          [(31) (fg "red")]
          [(32) (fg "green")]
          [(33) (fg "brown")]
          [(34) (fg "blue")]
          [(35) (fg "magenta")]
          [(36) (fg "cyan")]
          [(37) (fg "white")]
          [(38) (extended-color-handler term char (cdr params) #t)]
          [(39) null]
          [(40) (bg "black")]
          [(41) (bg "red")]
          [(42) (bg "green")]
          [(43) (bg "brown")]
          [(44) (bg "blue")]
          [(45) (bg "magenta")]
          [(46) (bg "cyan")]
          [(47) (bg "white")]
          [(48) (extended-color-handler term char (cdr params) #f)]
          [(49) (bg default-bg-color)]
          [else null])
        (color-csi-handler term char (cdr params) lq?))))

(define (extended-color-handler term char params fg?)
  (define setc (if fg?
                  set-terminal-current-fg-color!
                  set-terminal-current-bg-color!))
  (cond
    [(null? params) 'done]
    [(equal? (car params) 2)
     (if (< (length params) 4)
         null
         (begin
           (setc term (make-color (second params) (third params) (fourth params)))
           (color-csi-handler term char (list-tail params 4) #f)))]
    [(equal? (car params) 5)
     (if (< (length params) 2)
         null
         (begin
           (setc term (lookup-256color (second params)))
           (color-csi-handler term char (list-tail params 2) #f)))] ; TODO - add 256 color handling
    [else (color-csi-handler term char (cdr params) #f)]))

(define csi-table
  ;; a quick look at what codes are skipped with a trivial run of vim
  ;; H c h l
  ;; emacs
  ;; h c d H
  ;; less
  ;; H
  ;; H is set row,col
  ;; c is "tell me what kind of terminal you are"
  ;; h/l are set/reset various modes
  (hash
   #\@ (lambda (term char params lq?)
         (let ((n (car-defaulted params 1)))
           (terminal-insert-blank term n #t)))

   #\A (lambda (term char params lq?)
         (terminal-forward-lines term (- (car-defaulted params 1))))
   #\B (lambda (term char params lq?)
         (terminal-forward-lines term (car-defaulted params 1)))
   #\C (lambda (term char params lq?)
         (terminal-forward-chars term (car-defaulted params 1)))
   #\D (lambda (term char params lq?)
         (terminal-forward-chars term (- (car-defaulted params 1))))
   #\E (lambda (term char params lq?)
         (terminal-go-to-column term 0)
         (terminal-forward-lines term (car-defaulted params 1)))
   #\F (lambda (term char params lq?)
         (terminal-go-to-column term 0)
         (terminal-forward-lines term (- (car-defaulted params 1))))
   #\G (lambda (term char params lq?)
         (terminal-go-to-column term (sub1 (car-defaulted params 1))))
   #\H (lambda (term char params lq?)
         (terminal-go-to-row term (sub1 (car-defaulted params 1)))
         (terminal-go-to-column term (sub1 (cadr-defaulted params 1))))

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
   ;; X - erase n characters on current line -- meaning characters are replaced with spaces
   #\X (lambda (term char params lq?)
         (let ((n (car-defaulted params)))
           (terminal-delete-forward-at-cursor term n)
           (terminal-insert-blank term n)))

   ;; Half of these are duplicates. Stupid.
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
   ;; h - set mode
   ;; l - reset mode

   #\m color-csi-handler

   ;; n - status report
   ;; q - keyboard LEDs
   #\r (lambda (term char params lq?)
         (let ((start (sub1 (car-defaulted params 1)))
               (end (sub1 (cadr-defaulted params (terminal-current-height term)))))
           (terminal-set-scrolling-region term start end)))
   ;; s - save cursor location
   ;; u - restore cursor location
   #\` (lambda (term char params lq?)
         (terminal-go-to-column term (sub1 (car-defaulted params 1))))
   ))

