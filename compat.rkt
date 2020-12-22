#lang racket/base

(define-syntax-rule (fn args body)
  (lambda args body))

(provide fn)
