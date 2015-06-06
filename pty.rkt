#lang racket/base

;; Library for handling all the system calls to unix to set up a PTY

(require ffi/unsafe
         ffi/unsafe/define)
(require racket/list)

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


(require scheme/foreign)

(unsafe!)

(define scheme_make_fd_output_port
  (get-ffi-obj "scheme_make_fd_output_port" #f
               (_fun _int _scheme _int _int _int -> _scheme)))

(define scheme_get_port_file_descriptor
  (get-ffi-obj "scheme_get_port_file_descriptor" #f
               (_fun (port : _scheme) (fd : (_ptr o _int))
                     -> (ret : _int)
                     -> (if (ret . < . 0)
                            (error "couldn't get port file descriptor")
                            fd))))


(define (openpty [width 80] [height 24])
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
  (let ((ws (new-winsize width height)))
    (define-values (master slave slave-name) (openpty #f #f ws))
    (define-values (m-in m-out) (scheme_make_fd_output_port master "mastername" 0 0 1))
    (define-values (s-in s-out) (scheme_make_fd_output_port slave "slavename" 0 0 1))
    (values m-in m-out s-in s-out master slave)))


;; How can I get these from the .h file automatically, so this won't just break?
(define TIOCSWINSZ #x5414) ; set window size
(define TIOCGWINSZ #x5413) ; get window size
(define TIOCSCTTY #x540E) ; set controlling terminal
(define TIOCNOTTY #x5422) ; disown the controlling terminal

(define (set-pty-size fd winsize)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : _int)
                     (winsize : (_ptr i _winsize))
                     -> (ret : _int)
                     -> (when (< ret 0) (error "ioctl failed"))))
  (ioctl fd TIOCSWINSZ winsize))

(define (get-pty-size fd)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : _int)
                     (winsize : (_ptr o _winsize))
                     -> (ret : _int)
                     -> (if (< ret 0) (error "ioctl failed")
                            winsize)))
  (ioctl fd TIOCGWINSZ))


(define-ffi-definer define-libc (ffi-lib "libc" '("6" #f)))

(define argv-array-len 100)

(define-libc setsid (_fun -> (ret : _int)
                          -> (when (equal? ret -1) (error "setsid failed"))))

(define-libc setpgid (_fun (pid : _int) (pgid : _int)
                           -> (ret : _int)
                           -> (if (equal? ret -1)
                                  (error "setpgid failed!")
                                  ret)))


(define (execvp command . args)
  (define-libc execvp (_fun (file : _string) (argv : (_array/list _string argv-array-len))
                            -> (ret : _int)
                          -> (error "execvp failed")))
  (execvp command (append args (make-list (- argv-array-len (length args)) #f))))

(define (set-controlling-tty fd)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : _int)
                     _pointer
                     -> (ret : _int)
                     -> (when (equal? ret -1) (error "ioctl failed to set the controlling terminal"))))
  (ioctl fd TIOCSCTTY #f))

(define (disown-tty)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : _int)
                     _pointer
                     -> (ret : _int)
                     -> (when (equal? ret -1) (error "ioctl failed to disown controlling terminal"))))
  (ioctl (scheme_get_port_file_descriptor (current-input-port)) TIOCNOTTY #f))

