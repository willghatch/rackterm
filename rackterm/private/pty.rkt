#lang racket/base

;; Library for handling all the system calls to unix to set up a PTY

(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/port)
(require racket/list racket/port racket/system racket/string)

(provide (all-defined-out))

;; How can I get these from the .h file automatically, so this won't just break?
(define TIOCSWINSZ_gnu #x5414)
(define TIOCGWINSZ_gnu #x5413)
(define TIOCSCTTY_gnu #x540E)
(define TIOCNOTTY_gnu #x5422)

(define TIOCSWINSZ_freebsd #x80087467)
(define TIOCGWINSZ_freebsd #x40087468)
(define TIOCSCTTY_freebsd #x20007461)
(define TIOCNOTTY_freebsd #x20007471)

(define os-type (system-type 'os))
(define uname-s (if (equal? os-type 'windows)
                    "windows"
                    (string-trim (with-output-to-string
                                   (lambda () (system "uname -s"))))))
(define freebsd-ioctls? (or (string-ci=? uname-s "FreeBSD")
                            (string-ci=? uname-s "Darwin")))
;; ioctl request parameters are ints in Linux, but longs in FreeBSD and MacOSX
(define ioctl-req-type (if (or (equal? (system-type 'word) 64)
                               freebsd-ioctls?)
                           _long _int))

;; set window size
(define TIOCSWINSZ (if freebsd-ioctls? TIOCSWINSZ_freebsd TIOCSWINSZ_gnu))
;; get window size
(define TIOCGWINSZ (if freebsd-ioctls? TIOCGWINSZ_freebsd TIOCGWINSZ_gnu))
;; set controlling terminal
(define TIOCSCTTY (if freebsd-ioctls? TIOCSCTTY_freebsd TIOCSCTTY_gnu))
;; disown the controlling terminal
(define TIOCNOTTY (if freebsd-ioctls? TIOCNOTTY_freebsd TIOCNOTTY_gnu))

(define-ffi-definer define-pty (ffi-lib "libutil"))

(define-cstruct _winsize ([ws_row _ushort]
                          [ws_col _ushort]
                          ;; ws_xpixel and ws_ypixel are apparently unused...
                          [ws_xpixel _ushort]
                          [ws_ypixel _ushort]))
;; when the window size changes, a SIGWINCH signal is sent to the foreground process group

(define (new-winsize [width 80] [height 24])
  (make-winsize height width 0 0))




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
    (define-values (m-in m-out) (unsafe-file-descriptor->port master "mastername" '(read write)))
    (define-values (s-in s-out) (unsafe-file-descriptor->port slave "slavename" '(read write)))
    (values m-in m-out s-in s-out master slave)))



(define (set-pty-size fd winsize)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : ioctl-req-type)
                     (winsize : (_ptr i _winsize))
                     -> (ret : _int)
                     -> (when (< ret 0) (error "ioctl failed"))))
  (ioctl fd TIOCSWINSZ winsize))

(define (get-pty-size fd)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : ioctl-req-type)
                     (winsize : (_ptr o _winsize))
                     -> (ret : _int)
                     -> (if (< ret 0) (error "ioctl failed")
                            winsize)))
  (ioctl fd TIOCGWINSZ))


(define-ffi-definer define-libc (ffi-lib "libc" '("7" "6" #f)))

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
  (execvp command (append (list command)
                          args
                          (make-list (- argv-array-len 1 (length args)) #f))))

(define (set-controlling-tty fd)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : ioctl-req-type)
                     _pointer
                     -> (ret : _int)
                     -> (when (equal? ret -1) (error "ioctl failed to set the controlling terminal"))))
  (ioctl fd TIOCSCTTY #f))

(define (disown-tty)
  (define-pty ioctl (_fun
                     (fd : _int)
                     (request : ioctl-req-type)
                     _pointer
                     -> (ret : _int)
                     -> (when (equal? ret -1) (error "ioctl failed to disown controlling terminal"))))
  (ioctl (unsafe-port->file-descriptor (current-input-port)) TIOCNOTTY #f))

