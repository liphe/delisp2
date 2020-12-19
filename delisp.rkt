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
         [json-file (get-output-file file ".json")]
         [output-file (get-output-file file ".js")])
    (make-parent-directory* output-file)
    (display-to-file json-string json-file #:exists 'truncate)
    (system* "/Users/davazp/.nvm/versions/node/v12.18.2/bin/node" "format.js" json-file output-file)))

(define file-to-compile
  (command-line
   #:program "delisp"
   #:args (filename)
   filename))

(void 
 (compile-file file-to-compile))