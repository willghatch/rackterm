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


(define (compose-key-events e1 e2)
  (cond [(not e1) e2]
        [(not e2) e1]
        [else
         (apply make-key-event
                (for/list ((func (list key-event-char
                                       key-event-control
                                       key-event-meta
                                       key-event-super
                                       key-event-hyper
                                       key-event-shift)))
                  (or (map func (list e1 e2)))))]))

(define-struct key-tree
  (event-tree fallback-tree default-callback)
  #:transparent)

(define (at-least-one-aritize function)
  ;; the callbacks need to accept one argument for the character, but
  ;; aside from the default handler I bet most won't need it...
  (if (equal? 0 (procedure-arity function))
      (lambda (_) (function))
      function))

(define (key-tree-set ktree key-event-list callback)
  ;; functional update of key tree kt, which is a tree of hash tables
  (define (kt-set tree events callback)
    (define e (car events))
    (if (null? (cdr events))
        ;; this is the last in the chain of keys, so bind the callback here
        (hash-set tree e callback)
        ;; not the last in the list, so we need to add a sub-table
        (let ((current-item (hash-ref tree e #f)))
          (if (hash? current-item)
              ;; if the key already points to a sub-table, update it recursively
              (hash-set tree e (kt-set current-item (cdr key-event-list) callback))
              ;; key points to no table, change that.
              (hash-set tree e (kt-set (hash) (cdr key-event-list) callback))))))
  (let ((kt (key-tree-event-tree ktree))
        (events (if (key-event? key-event-list)
                    (list key-event-list)
                    key-event-list)))
    (struct-copy key-tree ktree
                 [event-tree (kt-set kt events callback)])))

(define (key-tree-set* ktree . args)
  (cond [(null? args) ktree]
        [(null? (cdr args)) (error "key-tree-set received a bad number of args")]
        [else (let ((keys (car args))
                    (callback (cadr args)))
                (apply key-tree-set* (cons (key-tree-set ktree keys callback)
                                           (cddr args))))]))

(define (empty-key-tree default-callback)
  (make-key-tree (hash) #f default-callback))

(define (key-tree-get ktree event)
  ;; returns either another key-tree or a callback function
  (let* ((dcb (key-tree-default-callback ktree))
         (fb-tree (key-tree-fallback-tree ktree))
         (ref (hash-ref (key-tree-event-tree ktree) event
                        (lambda () (if fb-tree
                                       (key-tree-get fb-tree event)
                                       dcb)))))
    (if (hash? ref)
        (make-key-tree ref fb-tree dcb)
        ref)))

(define (keyhandler default-handler . args)
  ;; makes a key tree, args are alternating key sequences and callbacks
  (let ((ktree (empty-key-tree  default-handler)))
    (apply key-tree-set* (cons ktree args))))

(define (keyhandler-with-fallback other-key-tree . args)
  ;; same as keyhandler, but fall back to another key tree
  (let ((ktree (make-key-tree (hash) other-key-tree #f)))
    (apply key-tree-set* (cons ktree args))))
