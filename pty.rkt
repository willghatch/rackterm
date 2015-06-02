#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(provide (all-defined-out))

(define-ffi-definer define-pty (ffi-lib "libutil"))

(define-cstruct _winsize ([ws_row _ushort]
                          [ws_col _ushort]
                          ;; ws_xpixel and ws_ypixel are apparently unused...
                          [ws_xpixel _ushort]
                          [ws_ypixel _ushort]))
;; when the window size changes, a SIGWINCH signal is sent to the foreground process group

(define (new-winsize [width 80] [height 24])
  (make-winsize height width 0 0))

;; name and termios can both be null, window size needs to be there, though.
(define-pty openpty (_fun (amaster : (_ptr o _int))
                          (aslave : (_ptr o _int))
                          ;(slave-name : (_ptr o _string))
                          (slave-name : _pointer)
                          (termios-ptr : _pointer)
                          (winsize : (_ptr i _winsize))
                          -> (r : _int)
                          -> (if (< 0 r)
                                 (error "openpty failed")
                                 (values amaster aslave slave-name))))


(require scheme/foreign)

(unsafe!)

(define scheme_make_fd_output_port
  (get-ffi-obj "scheme_make_fd_output_port" #f
               (_fun _int _scheme _int _int -> _scheme)))
(define scheme_make_fd_input_port
  (get-ffi-obj "scheme_make_fd_input_port" #f
               (_fun _int _scheme _int _int -> _scheme)))




(define (my-openpty [width 80] [height 24])
  (let ((ws (new-winsize width height)))
    (define-values (master slave slave-name) (openpty #f #f ws))
    (define-values (m-in m-out) (scheme_make_fd_output_port master "mastername" 0 0))
    (define-values (s-in s-out) (scheme_make_fd_output_port slave "slavename" 0 0))
    (values m-in m-out s-in s-out master)))

(define-pty ioctl (_fun
                   (fd : _int)
                   (request : _int)
                   (winsize : (_ptr io _winsize))
                   -> (ret : _int)
                   -> (if (< ret 0) (error "ioctl failed")
                          winsize)))


;; How can I get these from the .h file automatically, so this won't just break?
(define TIOCSWINSZ #x5413) ; set window size
(define TIOCGWINSZ #x5414) ; get window size

(define (set-pty-size fd winsize)
  (ioctl fd TIOCSWINSZ winsize))

(define (get-pty-size fd winsize)
  (ioctl fd TIOCGWINSZ winsize))



(define-ffi-definer define-libc (ffi-lib "libc" '("6" #f)))

(define-libc fork (_fun -> (ret : _int)))

(define-libc execvp (_fun (file : _string) (argv : _pointer)
                          -> (ret : _int)
                          -> (error "execvp failed")))
(define (make-argv args)
  (_array _string (add1 (length args)) (append args (list #f))))
(define (rexecvp command . args)
  (execvp command (make-argv args)))

(define (exec-with-new-tty fd command . args)
  ;; set terminal here with fd
  (rexecvp command args))

(define (subproc-with-new-controlling-tty tty-fd command . args)
  (case (fork)
    [(-1) (error "fork failed")]
    [(0) (void)] ;; parent
    [else (apply exec-with-new-tty (append (list tty-fd command) args))]))

