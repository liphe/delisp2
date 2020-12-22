#lang racket/base

(require "compat.rkt")

(require racket/match)
(require json)

;; Symbol handling

(define get-symbol-map
  (make-parameter 5))

(define ast
  (fn (type . args)
    (apply hash 'type type args)))

;; Expressions

(define literal?
  (fn (expr)
    (or (integer? expr)
        (string? expr)
        (boolean? expr))))

(define compile-expr
  (fn (expr)
    (match expr
      [(? literal?)
       (compile-literal expr)]
      [(? symbol?)
       (compile-variable expr)]
      [(list 'quote (and symbol (? symbol?)))
        (compile-symbol symbol)]
      [(list 'if condition consequent alternate)
       (compile-conditional condition consequent alternate)]
      [(list 'fn (list args ...) body)
       (compile-fn args body)]
      [(list 'record (list ks vs) ...)
       (compile-record ks vs)]
      [(list 'let (list (list ks vs) ...) body)
       (compile-let ks vs body)]
      [(list fn args ...)
       (compile-funcall fn args)]
      )))

(define definition (fn (name value) (ast "VariableDeclaration"
     'kind "const"
     'declarations (list (ast "VariableDeclarator"
                          'id name
                          'init value)))))

(define string->jsidentifier
  (fn (name)
    (ast "Identifier"
         'name name)))

(define identifier
  (fn (name)
    (string->jsidentifier (symbol->string name))))


(define compile-literal
  (fn (value)
    (ast "Literal"
         'value value
         'raw (let ([o (open-output-string)])
                (write value o)
                (get-output-string o)))))

(define compile-variable
  (fn (variable)
    (identifier variable)))

(define compile-symbol
  (fn (sym)
    (ast "Identifier"
      'name (hash-ref! (get-symbol-map) sym
        (fn () (string-append "__symbol_" (symbol->string sym)))))))

(define compile-conditional
  (fn (test consequent alternate)
    (ast "ConditionalExpression"
         'test test
         'consequent consequent
         'alternate alternate)))

(define compile-fn
  (fn (args body)
    (ast "ArrowFunctionExpression"
         'params (map identifier args)
         'body (compile-expr body))))

(define compile-record
  (fn (keys values)
    (ast "ObjectExpression"
         'properties (for/list ([k keys] [v values]) (ast "Property"
                                                          'key (identifier k)
                                                          'value (compile-expr v))))))

(define compile-let
  (fn (vars values body)
    (compile-funcall (list 'fn vars body) values)))

(define compile-funcall
  (fn (fn args)
    (ast "CallExpression"
         'callee (compile-expr fn)
         'expression #t
         'arguments (map compile-expr args))))

;; Toplevel forms
(define compile-symbol-list
  (fn ()
    (for/list ([(name id) (get-symbol-map)])
      (definition (string->jsidentifier id)
        (ast "CallExpression"
        'callee (string->jsidentifier "Symbol")
        'arguments (list (compile-expr (symbol->string name))))))))

(define compile-toplevel
  (fn (form)
    (match form
      [(list 'define name expr)
       (definition (identifier name) (compile-expr expr))]
      [_
       (compile-expr form)]
      )))


(define compile-module
  (fn (m)
    (parameterize ([get-symbol-map (make-hash)])
     (let ([body (map compile-toplevel m)])
            (jsexpr->string
             (ast "Program"
                  'sourceType "module"
                  'body (append (compile-symbol-list) body)))))))

(provide compile-module)
