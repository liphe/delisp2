#lang racket/base

(require racket/match)
(require json)

(define (ast type . args)
  (apply hash 'type type args))

;; Expressions

(define (literal? expr)
  (or (integer? expr)
      (string? expr)
      (boolean? expr)))

(define (compile-expr expr)
  (match expr
    [(? literal?)
     (compile-literal expr)]
    [(? symbol?)
     (compile-variable expr)]
    [(list 'if condition consequent alternate)
     (compile-conditional condition consequent alternate)]
    [(list 'fn (list args ...) body)
     (compile-fn args body)]
    [(list fn args ...)
     (compile-funcall fn args)]
    ))

(define (identifier name)
  (ast "Identifier"
       'name (symbol->string name)))

(define (compile-literal value)
   (ast "Literal"
        'value value
        'raw (let ([o (open-output-string)])
               (write value o)
               (get-output-string o))))

(define (compile-variable variable)
  (identifier variable))

(define (compile-conditional test consequent alternate)
  (ast "ConditionalExpression"
       'test test
       'consequent consequent
       'alternate alternate))

(define (compile-fn args body)
  (ast "ArrowFunctionExpression"
       'params (map identifier args)
       'body (compile-expr body)))

(define (compile-funcall fn args)
  (ast "CallExpression"
       'callee (compile-expr fn)
       'expression #t
       'arguments (map compile-expr args)))


;; Toplevel forms

(define (compile-toplevel form)
  (match form
    [(list 'define name expr)
     (ast "VariableDeclaration"
          'kind "const"
          'declarations (list (ast "VariableDeclarator"
                                   'id (identifier name)
                                   'init (compile-expr expr))))]
    [_
     (compile-expr form)]
    ))


(define (compile-module m)
  (jsexpr->string
   (ast "Program"
        'sourceType "module"
        'body (map compile-toplevel m))))

(provide compile-module)
