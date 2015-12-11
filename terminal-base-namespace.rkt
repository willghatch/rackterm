#lang racket/base

;; This provides all the non-terminal primitives for terminal evaluation.
;; Terminal-specific primitives will be added on to the namespace with the specific
;; terminal they apply to.

(provide
 #%app
 #%datum
 begin
 define
 lambda
 λ
 let
 apply
 )
