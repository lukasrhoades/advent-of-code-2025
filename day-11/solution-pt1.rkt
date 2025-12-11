#!/usr/bin/env racket
#lang racket/base

(require racket/string)

(define in (open-input-file "input.txt"))

(define (file-reader file stop proc res)
  (let ((new-line (read-line file)))
    (if (stop new-line)
        res
        (file-reader file stop proc (append res (proc new-line))))))

(define input
  (file-reader in
               eof-object?
               (lambda (x)
                 (list (cons (cadr
                               (regexp-match #px"\\b(\\D{3}):" x))
                             (list
                               (string-split
                                 (string-trim x #px"\\b\\D{3}:")
                                 " ")))))
               '()))

(define (pathfind curr graph)
  (if (equal? curr "out")
      1
      (sum (map (lambda (x)
                  (pathfind x graph))
                (get-neighbors curr graph)))))
(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (get-neighbors key graph)
  (cond ((null? graph) '())
        ((equal? key (caar graph))
         (cadar graph))
        (else (get-neighbors key (cdr graph)))))

(pathfind "you" input)
