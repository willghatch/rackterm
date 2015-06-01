#lang racket/base

(require ffi/unsafe
         ffi/unsafe/define)

(provide (all-defined-out))

;(define-ffi-definer define-pty (ffi-lib "pty.h"))
;(define-ffi-definer define-pty (ffi-lib "libc"))
;(define-ffi-definer define-pty (ffi-lib "libc.so.6"))
(define-ffi-definer define-pty (ffi-lib "libutil"))

(define-cstruct _winsize ([ws_row _ushort]
                          [ws_col _ushort]
                          ;; ws_xpixel and ws_ypixel are apparently unused...
                          [ws_xpixel _ushort]
                          [ws_ypixel _ushort]))
;; when the window size changes, a SIGWINCH signal is sent to the foreground process group

(define (new-winsize [width 80] [height 24])
  (make-winsize width height 0 0))

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

;(define ffi_fd->ports
;  (get-ffi-obj "scheme_make_fd_output_port" #f
;               (_fun _int _scheme _int _int -> _scheme)))
;
;(define (fd->ports fd name)
;  ;; fd, fd-name, regfile?, win_textmode?, read_too?
;  (ffi_fd->ports fd name 0 0 1))
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
    (values m-in m-out s-in s-out)))


;; in termios.h
;; TIOCGWINSZ -- get window size
;; TIOCSWINSZ -- set window size
;; (both take a pointer to a winsize, and getting sets that struct)
