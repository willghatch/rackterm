#lang racket

(provide (all-defined-out))

(define-struct key-event
  (char
   control
   meta
   super
   hyper
   shift
   ;; Some sort of configuration should tell the gui classes how to map mod1-5 to
   ;; meta, super, hyper...
   )
  #:transparent)

(define (no-mods-key-event key)
  (make-key-event key
                  #f #f #f #f #f))

(define key-symbols
  '(backtab menu pause prior next end home left right up down escape print insert
            numpad0 numpad1 numpad2 numpad3 numpad4 numpad5 numpad6 numpad7 numpad8 numpad9
            numpad-enter multiply add subtract decimal divide
            f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
            wheel-up wheel-down wheel-left wheel-right
    ))

(define (key . args)
  ;; convenience function to say eg. (key 'meta #\a)
  (define (mem x xs)
    ;; to standardize true values to #t
    (if (member x xs) #t #f))
  (define (find-key-symbol syms)
    (cond [(null? syms) #f]
          [(member (car syms) key-symbols) (car syms)]
          [else (find-key-symbol (cdr syms))]))
  (let* ((fchar (memf char? args))
         (char (if fchar (car fchar) #f))
         (ch-sym (if char char
                     ;; if it's not a char, look for one of the special key symbols
                     (find-key-symbol args))))
    (make-key-event ch-sym
                    (mem 'control args)
                    (mem 'meta args)
                    (mem 'super args)
                    (mem 'hyper args)
                    (mem 'shift args))))

(define/contract (keys . args)
  ;; convenience to flatten key lists make with (key ...)
  (-> (listof (or/c key-event? (listof key-event?)))
      (listof key-event?))
  (flatten args))

(define (at-least-one-aritize function)
  ;; the callbacks need to accept one argument for the character, but
  ;; aside from the default handler I bet most won't need it...
  (if (equal? 0 (procedure-arity function))
      (lambda (_) (function))
      function))

(define (get-handler-for-keymap keymap k-ev default)
  (cond
    [(dict-has-key? keymap k-ev) (dict-ref keymap k-ev)]
    [(dict-has-key? keymap 'default) (dict-ref keymap 'default)]
    [else default]))

(define (fall-back-to-other-keymap keymap default-handler)
  (λ (k-ev) ((get-handler-for-keymap keymap
                                     k-ev
                                     default-handler)
             k-ev)))

(define (make-keymap . args)
  ;; This should take key-events as keys (or the symbol 'default) and have functions that take a key-event as arguments
  (define (recur ht args)
    (cond [(null? args) ht]
          [(null? (cdr args)) (error "uneven argument list for keyhandler")]
          [else (begin (hash-set! ht (car args) (cadr args))
                       (recur ht (cddr args)))]))
  (recur (make-hash) args))
