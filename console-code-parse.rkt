#lang racket/base
(require racket/block)
(require racket/list)
(require racket/match)

(provide parse-char
         parse-string)

#|

All handlers return (values <next handler> <output from parsing this character>)
They return #f for the default handler, and '() for empty output.

This parser is a piece of junk that I threw together.  But I would
like to make an actually decent modular and extensible parser.

Will I ever get around to it?  Maybe.

The main constraint for the parser is that it has to be able to return
a partial parse one character at a time, giving the bit of output it
can determine from that character, and the new state of the parser.
That way it can be used to live-parse console codes for a terminal
emulator, which needs to interpret them while they are coming in.

I would also like to make better convenience functions for various use
cases.  Mainly I would like to be able to squash repeated character
output functions into a single string output function and squash
top-level begins.  I would also like to be able to specify which
non-printing codes I care about and filter down to those ones, so an
application may get styling information for some strings, but ignore
everything else.

|#

#|
TODO
I need to standardize the function names here.  These ones are based
on what I just happened to have defined in my terminal implementation,
but the API deserves good names.  Then I should document them.

|#

(define (default-handler char)
  (if ((char->integer char) . < . 32)
      (handle-ascii-controls char)
      (values #f `(terminal-write-char ,char))))

(define (handle-ascii-controls char)
  (case char
    [(#\u07) (values #f '(bell))] ;; BEEP!
    [(#\u08) (values #f '(terminal-forward-chars -1))] ;; backspace
    [(#\u09) (values #f '(terminal-go-to-next-tab-stop))]
    [(#\newline #\u0B #\u0C) (values #f '(terminal-newline))]
    [(#\return) (values handle-post-return '())] ;; carriage return...
    [(#\u0E) (values #f '(activate-g1-character-set))] ;; activate G1 character set
    [(#\u0F) (values #f '(activate-g0-character-set))] ;; activate G0 character set
    [(#\u1B) (values escape-handler '())] ;; start escape sequence
    [(#\u9B) (values new-csi-handler '())]
    [else (values #f `(unknown-control-character ,char))]))

(define (handle-post-return char)
  (case char
    [(#\newline) (values #f '(terminal-crlf))]
    [else (let-values ([(handler out) (default-handler char)])
            (if (null? out)
                (values handler '(terminal-return))
                (values handler `(begin (terminal-return) ,out))))]))

(define (escape-handler char)
  ;; IE handling after receiving ESC character
  (case char
    [(#\D) (values #f '(terminal-forward-lines 1))]
    [(#\H) (values #f '(terminal-set-tab-stop))]
    ;; M should scroll up one line.  If at the top, it should remove the bottom line and insert one
    ;; TODO - check if this was the same as what I'm calling cursor-move-line or different (IE some weird scroll-region stuff might be here but I don't remember...)
    [(#\M) (values #f '(terminal-do-esc-M))]
    [(#\[) (values new-csi-handler '())]
    [(#\]) (values new-osc-handler '())]

    ;; for setting 7 or 8 bit controls, ascii conformance...
    [(#\space) (values (ignore-next-escaped-char char) '())]

    ;;; The rest are less important
    [(#\#) (values (ignore-next-escaped-char char) '())]
    [(#\%) (values (ignore-next-escaped-char char) '())]
    [(#\+) (values (ignore-next-escaped-char char) '())]
    [(#\-) (values (ignore-next-escaped-char char) '())]
    [(#\*) (values (ignore-next-escaped-char char) '())]
    [(#\/) (values (ignore-next-escaped-char char) '())]
    [(#\.) (values (ignore-next-escaped-char char) '())]


    ;; these paren ones have something to do with setting character sets
    [(#\() (values (ignore-next-escaped-char char) '())]
    [(#\)) (values (ignore-next-escaped-char char) '())]
    [else (values #f `(unknown-escape-character ,char))]))

(define (make-osc-handler numeric-arg)
  ;; osc sequences are "ESC ] number ; <usually text> <ST - string terminator>
  (lambda (char)
    (case char
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (values (make-osc-handler (+ (* 10 numeric-arg)
                                    (read (open-input-string (string char)))))
               '())]
      ;; the only other character should be the semicolon
      [else (values (make-osc-text-handler numeric-arg "" #f) '())])))
(define new-osc-handler (make-osc-handler 0))
(define (make-osc-text-handler numeric-arg text previous-char)
  (lambda (char)
    ;; the string terminator is ESC-\
    ;; but xterm seems to support just #\u07 (ascii BELL)
    (cond ((and (equal? char #\\)
                (equal? previous-char #\u1B))
           (osc-handler-finish numeric-arg text))
          ((equal? char #\u07)
           (osc-handler-finish numeric-arg (string-append text (string previous-char))))
          (else (values (let ((new-text (if previous-char
                                            (string-append text (string previous-char))
                                            "")))
                          (make-osc-text-handler numeric-arg
                                                 new-text
                                                 char))
                        '())))))

(define (read/str->list str)
  (define (recur port cur)
    (let ((out (read port)))
      (if (equal? out eof)
          cur
          (recur port (cons out cur)))))
  (let ((p (open-input-string str)))
    (reverse (recur p '()))))

(define (osc-handler-finish numeric-arg text)
  (case numeric-arg
    [(0 1 2) (values #f `(set-terminal-title! ,text))]
    [(3) (values #f '())] ; this should set X properties.
    [(99931337) (values #f `(begin ,@(read/str->list text)))] ; eval whatever!
    [else (values #f '())])) ; aaaand some other stuff.

(define (make-ignore-next-n-escape-sequence-handler so-far n)
  (lambda (char)
    (if (n . < . 2)
        (values #f `(ignored-escape-sequence (quote ,(reverse (cons char so-far)))))
        (values (make-ignore-next-n-escape-sequence-handler (cons char so-far) (sub1 n)) '()))))
(define (ignore-next-escaped-char start-char)
  (make-ignore-next-n-escape-sequence-handler (list start-char) 1))

(define (make-csi-handler completed-params current-param leading-question?)
  ;; CSI sequences start with 'ESC [', then possibly a question mark (which seems
  ;; to only matter for setting/resetting modes with h/l, in which case it means
  ;; "private mode"), then decimal number arguments separated by semicolons,
  ;; until a final character that determines the function.
  (lambda (char)
    (case char
      [(#\;) (values (make-csi-handler
                      (append completed-params (list current-param))
                      0
                      leading-question?)
                     '())]
      [(#\?) (if (and (null? completed-params) (equal? current-param 0))
                 (values (make-csi-handler
                          completed-params
                          current-param
                          #t)
                         '())
                 (values #f '()))] ; I guess a question mark somewhere else just kills it...?
      [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
       (values (make-csi-handler
                completed-params
                (+ (* 10 current-param)
                   (read (open-input-string (string char))))
                leading-question?)
               '())]
      [else
       (let* ((final-params (append completed-params (list current-param)))
              (end-handler
               (hash-ref csi-table char
                         (lambda ()
                           (lambda (char params lq?)
                             (values #f
                                     `(unknown-csi-terminator
                                       ,char (quote ,final-params))))))))

         (end-handler char final-params leading-question?))])))

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

(define (filter-nulls xs)
  (filter (λ (x) (not (null? x)))
          xs))

(define (handle-set-mode char params private? output-so-far)
  ;; reset if l, set if h
  (if (null? params)
      (values #f `(begin ,@(filter-nulls (reverse output-so-far))))
      (block
        (define on? (equal? char #\h))
        (define (recur o-s-f)
          (handle-set-mode char (cdr params) private? (cons o-s-f output-so-far)))
        (define setting (car-defaulted params 0))
        (define (mk-ignore)
          `(unknown-mode-set ,setting ,private? ,on?))
        (if private?
            (case setting
              [(6) (recur `(set-terminal-margin-relative-addressing! ,on?))]
              [(25) (recur `(set-terminal-cursor-visible! ,on?))]
              [(1049) (recur `(set-terminal-current-alt-screen-state! ,on?))]
              [else (recur (mk-ignore))])
            (case setting
              [else (recur (mk-ignore))])))))

(define (color-csi-handler params lq? output-so-far)
  ;; TODO - check all the ones listed on the wikipedia page for ansi escape codes...
  ;; there are a lot of obscure ones
  ;; 24 bit color = CSI-38;2;r;g;bm for fg and 48 instead of 38 for bg
  ;; for 256 color pallete, CSI-38;5;colorm
  (define (recur o-s-f)
    (color-csi-handler (cdr params) lq? (cons o-s-f output-so-far)))
  (define (fg color)
    (recur `(set-style-fg-color! ,color)))
  (define (bg color)
    (recur `(set-style-bg-color! ,color)))
  (if (null? params)
      (values #f `(begin ,@(filter-nulls (reverse output-so-far))))
      (case (car params)
        [(0) (recur '(set-style-default!))]
        [(1) (recur '(set-style-bold! #t))]
        ;[(2) null]
        [(3) (recur '(set-style-italic! #t))]
        [(4) (recur '(set-style-underline! #t))]
        [(5) (recur '(set-style-blink! #t))]
        [(7) (recur '(set-style-reverse-video! #t))]
        ;[(10) null]
        ;[(11) null]
        ;[(12) null]
        ;[(21) null]
        [(22) (recur '(set-style-bold! #f))]
        [(23) (recur '(set-style-italic! #f))]
        [(24) (recur '(set-style-underline! #f))]
        [(25) (recur '(set-style-blink! #f))]
        [(27) (recur '(set-style-reverse-video! #f))]
        [(30) (fg ''black)]
        [(31) (fg ''red)]
        [(32) (fg ''green)]
        [(33) (fg ''brown)]
        [(34) (fg ''blue)]
        [(35) (fg ''magenta)]
        [(36) (fg ''cyan)]
        [(37) (fg ''white)]
        [(38) (extended-color-handler (cdr params) lq? #t output-so-far)]
        [(39) (fg ''default-fg)]
        [(40) (bg ''black)]
        [(41) (bg ''red)]
        [(42) (bg ''green)]
        [(43) (bg ''brown)]
        [(44) (bg ''blue)]
        [(45) (bg ''magenta)]
        [(46) (bg ''cyan)]
        [(47) (bg ''white)]
        [(48) (extended-color-handler (cdr params) lq? #f output-so-far)]
        [(49) (bg ''default-bg)]
        [else (recur '())])))

(define (extended-color-handler params lq? fg? output-so-far)
  (define setc (if fg? 'set-style-fg-color! 'set-style-bg-color!))
  (define (finish) (color-csi-handler null lq? output-so-far))
  (cond
    [(null? params) (finish)]
    [(equal? (car params) 2)
     (if (< (length params) 4)
         (finish)
         (color-csi-handler (list-tail params 4)
                            lq?
                            (cons `(,setc ,(second params) ,(third params) ,(fourth params))
                                  output-so-far)))]
    [(equal? (car params) 5)
     (if (< (length params) 2)
         (finish)
         (color-csi-handler (list-tail params 2)
                            lq?
                            (cons `(,setc ,(second params))
                                  output-so-far)))]
    [else (color-csi-handler (cdr params) lq? output-so-far)]))

(define csi-table
  (hash
   ;; insert blanks
   #\@ (lambda (char params lq?)
         (let ((n (car-defaulted params 1)))
           (values #f `(insert-blanks ,n))))

   ;; forward lines
   #\A (lambda (char params lq?)
         (values #f `(terminal-forward-lines ,(- (car-defaulted params 1)))))
   #\B (lambda (char params lq?)
         (values #f `(terminal-forward-lines ,(car-defaulted params 1))))

   ;; forward chars
   #\C (lambda (char params lq?)
         (values #f `(terminal-forward-chars ,(car-defaulted params 1))))
   #\D (lambda (char params lq?)
         (values #f `(terminal-forward-chars ,(- (car-defaulted params 1)))))

   ;; forward lines to column 0
   #\E (lambda (char params lq?)
         (values #f `(terminal-forward-lines-column-0 ,(car-defaulted params 1))))
   #\F (lambda (char params lq?)
         (values #f `(terminal-forward-lines-column-0 ,(- (car-defaulted params 1)))))

   ;; go to address directly
   #\G (lambda (char params lq?)
         (values #f `(terminal-go-to-column ,(sub1 (car-defaulted params 1)))))
   #\H (lambda (char params lq?)
         (values #f `(terminal-go-to-row-column ,(sub1 (car-defaulted params 1))
                                       ,(sub1 (cadr-defaulted params 1)))))

   ;; clear screen
   #\J (lambda (char params lq?)
         (let ((n (car params)))
           (case n
             ;; 3 is supposed to clear including the scrollback buffer in the Linux
             ;;   terminal, but I don't think I care for that feature.
             [(2) (values #f '(terminal-clear))]
             [(1) (values #f '(terminal-clear-from-start-to-cursor))]
             ;; 0
             [else (values #f '(terminal-clear-from-cursor-to-end))])))

   ;; clear line
   #\K (lambda (char params lq?)
         (let ((n (car params)))
           (case n
             [(2) (values #f '(terminal-clear-current-line))]
             [(1) (values #f '(terminal-clear-from-start-of-line-to-cursor))]
             ;; 0
             [else (values #f '(terminal-delete-to-end-of-line))])))

   ;; L - insert n blank lines
   #\L (lambda (char params lq?)
         (values #f `(terminal-insert-lines-with-scrolling-region ,(car-defaulted params 1))))
   ;; M - delete n lines
   #\M (lambda (char params lq?)
         (values #f `(terminal-delete-lines-with-scrolling-region ,(car-defaulted params 1))))
   ;; P - delete n characters on current line -- meaning characters shift left
   #\P (lambda (char params lq?)
         (values #f `(terminal-delete-forward-at-cursor ,(car-defaulted params 1))))
   ;; S scroll up n lines
   #\S (lambda (char params lq?)
         (values #f `(terminal-scroll-region ,(car-defaulted params 1))))
   ;; T scroll down n lines
   #\T (lambda (char params lq?)
         (values #f `(terminal-scroll-region ,(- (car-defaulted params 1)))))
   ;; X - erase n characters on current line -- meaning characters are replaced with spaces
   #\X (lambda (char params lq?)
         (values #f `(terminal-replace-chars-with-space ,(car-defaulted params 1))))

   ;; Half of these are duplicates. Stupid.
   ;; Forward chars
   #\a (lambda (char params lq?)
         (values #f `(terminal-forward-chars ,(car-defaulted params 1))))
   ;; b... I don't see a spec for it
   ;; c -- some sort of terminal identification...

   #\d (lambda (char params lq?)
         (values #f `(terminal-go-to-row ,(sub1 (car-defaulted params 1)))))
   #\e (lambda (char params lq?)
         (values #f `(terminal-forward-lines ,(car-defaulted params 1))))
   #\f (lambda (char params lq?)
         (values #f `(terminal-go-to-row-column ,(sub1 (car-defaulted params 1))
                                       ,(sub1 (cadr-defaulted params 1)))))

   ;; g - 0 - clear tab stop at current position
   ;;     3 - delete all tab stops
   #\g (lambda (char params lq?)
         (let ((arg (car-defaulted params 1)))
           (if (equal? arg 3)
               (values #f '(terminal-remove-all-tab-stops))
               (values #f '(terminal-remove-tab-stop)))))

   ;; h - set mode
   ;; l - reset mode
   #\h (lambda (char params lq?)
         (handle-set-mode char params lq? '()))
   #\l (lambda (char params lq?)
         (handle-set-mode char params lq? '()))


   #\m (lambda (char params lq?)
         (color-csi-handler params lq? '()))

   ;; n - status report
   ;; q - keyboard LEDs

   ;; set scrolling region
   #\r (lambda (char params lq?)
         (values #f `(terminal-set-scrolling-region ,(car-defaulted params 1)
                                                    ,(cadr-defaulted params ''end))))

   ;; s - save cursor location
   ;; u - restore cursor location


   #\` (lambda (char params lq?)
         (values #f `(terminal-go-to-column ,(sub1 (car-defaulted params 1)))))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parse functions


(define (parse-char c #:parser-state [init-handler #f])
  (if init-handler
      (init-handler c)
      (default-handler c)))

(define (parse-string s #:parser-state [init-handler #f])
  (let-values
      ([(handler-out r-outputs)
        (for/fold ([handler (or init-handler default-handler)]
                   [outputs '()])
                  ([char s])
          (let-values ([(next-handler result) (handler char)])
            (values (or next-handler default-handler)
                    (if (null? result)
                        outputs
                        (cons result outputs)))))])
    (values handler-out (reverse r-outputs))))

(define (squash-begins results)
  (match results
    ['() '()]
    [(list-rest (list-rest 'begin b-forms) forms-rest)
     (append b-forms (squash-begins forms-rest))]
    [(list-rest x xs) (cons x (squash-begins xs))]))

(define (squash-write-char results)
  (define (rec forms cur-chars)
    (match forms
      ['() (if (null? cur-chars)
               '()
               `((terminal-write-string ,(apply string (reverse cur-chars)))))]
      [(list-rest `(terminal-write-char ,c) r-forms)
       (rec r-forms (cons c cur-chars))]
      [else (if (null? cur-chars)
                (cons (car forms) (rec (cdr forms) cur-chars))
                (cons `(terminal-write-string ,(apply string (reverse cur-chars)))
                      (rec forms '())))]))
  (rec results '()))

(define (parse-string/squash s #:parser-state [init-handler #f])
  (let-values ([(parser-state output) (parse-string s #:parser-state init-handler)])
    (values parser-state (squash-write-char (squash-begins output)))))

(define (parse-string/no-state s)
  (let-values ([(parser-state output) (parse-string/squash s)])
    output))

(define (parse-results->bare-string squashed-results)
  (let ([strs (map (λ (r) (cond [(equal? (car r) 'terminal-write-string) (cadr r)]
                                [else "\n"]))
                   (filter (λ (r) (or (equal? (car r) 'terminal-write-string)
                                      (equal? (car r) 'terminal-newline)
                                      (equal? (car r) 'terminal-crlf)
                                      ;; I think I don't want to have both \r\n in...
                                      ;;(equal? (car r) 'terminal-return)
                                      ))
                           squashed-results))])
    (apply string-append strs)))

(define (parse-out-ansi s)
  ;; remove ansi codes and get bare strings back
  (parse-results->bare-string (parse-string/no-state s)))

(module+ test
  (require rackunit)

  (define (p s)
    (define-values (handler output) (parse-string s))
    output)

  (check-equal? (p "test")
                '((terminal-write-char #\t)
                  (terminal-write-char #\e)
                  (terminal-write-char #\s)
                  (terminal-write-char #\t)))
  (check-equal? (parse-string/no-state "test")
                '((terminal-write-string "test")))
  (check-equal? (p "\a")
                '((bell)))
  (define color-string "\033[32;41mtesting\033[5;4;38;2;33;55;127m colors\033[0m")
  (check-equal? (p color-string)
                '((begin (set-style-fg-color! 'green)
                         (set-style-bg-color! 'red))
                  (terminal-write-char #\t)
                  (terminal-write-char #\e)
                  (terminal-write-char #\s)
                  (terminal-write-char #\t)
                  (terminal-write-char #\i)
                  (terminal-write-char #\n)
                  (terminal-write-char #\g)
                  (begin
                    (set-style-blink! #t)
                    (set-style-underline! #t)
                    (set-style-fg-color! 33 55 127))
                  (terminal-write-char #\space)
                  (terminal-write-char #\c)
                  (terminal-write-char #\o)
                  (terminal-write-char #\l)
                  (terminal-write-char #\o)
                  (terminal-write-char #\r)
                  (terminal-write-char #\s)
                  (begin (set-style-default!))))
  (check-equal? (parse-string/no-state color-string)
                '((set-style-fg-color! 'green)
                  (set-style-bg-color! 'red)
                  (terminal-write-string "testing")
                  (set-style-blink! #t)
                  (set-style-underline! #t)
                  (set-style-fg-color! 33 55 127)
                  (terminal-write-string " colors")
                  (set-style-default!)))
  (check-equal? (parse-out-ansi color-string)
                "testing colors")
  )
