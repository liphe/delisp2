#lang racket/base

(require racket/file
         racket/match
         racket/cmdline
         racket/system
         racket/path)

(require "compiler.rkt")

(define project-root
  (simple-form-path (path-only (find-system-path 'run-file))))

(define output-directory
  (build-path project-root ".delisp" "builds"))

(define (get-output-file input-file ext)
  (let ([relative (find-relative-path project-root (simple-form-path input-file))])
    (build-path output-directory (path-replace-extension relative ext))))

;; Compile a file.
;;
;; All IO is confined in this file.
(define (compile-file file)
  (let* ([m (file->list file)]
         [json-string (compile-module m)]
         [output-file (get-output-file file ".js")])
    (make-parent-directory* output-file)
    (format-js-to-file json-string output-file)))

(define (format-js-to-file ast out)
  (let ([json-file (make-temporary-file)])
    (display-to-file ast json-file #:exists 'truncate)
    (let ([node (find-executable-path "node")])
      (system* node "format.js" json-file out))))

(define (format-js ast)
  (format-js-to-file ast "-"))

(define file-to-compile
  (command-line
   #:program "delisp"
   #:args (filename)
   filename))

(void 
 (compile-file file-to-compile))
