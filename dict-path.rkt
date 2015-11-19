#lang racket/base
(require racket/dict)

(define (dict-ref/path hash on-fail . keys)
  (cond [(null? keys) hash]
        [(not (dict? hash)) (if (procedure? on-fail) (on-fail) on-fail)]
        [else (apply dict-ref/path (list* (dict-ref hash (car keys) on-fail) on-fail (cdr keys)))]))

(define (dict-set/path h v key1 . rest-keys)
  (if (null? rest-keys)
      (dict-set h key1 v)
      (let ((ref (dict-ref h key1 #f))
            (key2 (car rest-keys))
            (keys (cdr rest-keys)))
        (if (dict? ref)
            (dict-set h key1 (apply dict-set/path (list* ref v key2 keys)))
            (let ((new-hash (hash)))
              (dict-set h key1 (apply dict-set/path (list* new-hash v key2 keys))))))))

(module+ test
  (require rackunit)
  (define empty-hash (hash))
  (define a (dict-set/path empty-hash 1 'foo 'bar 'aoeu))
  (define b (dict-set/path a 2 'foo 'qwer 'asdf))
  (check-equal? (dict-ref/path b #f 'foo 'bar 'aoeu) 1)
  (check-equal? (dict-ref/path b #f 'foo 'qwer) (hash 'asdf 2))
  (check-equal? (dict-ref/path b 'default 'foo 'bad-path 'bar) 'default)
  )
