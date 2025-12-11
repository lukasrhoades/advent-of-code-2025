#!/usr/bin/env racket
#lang racket/base

(require racket/string)

(define in (open-input-file "input.txt"))

(define (file-reader file res proc stop)
  (let ((line (read-line file)))
    (if (stop line)
        res
        (file-reader file (append res (proc line)) proc stop))))

(define ranges
  (file-reader in
               '()
               (lambda (x) (list (string-split x "-")))
               (lambda (x) (equal? x ""))))

(define ids (file-reader in '() list eof-object?))

(define (fresh-counter ids ranges)
  (sum (map (lambda (x) (test x ranges)) ids)))

(define (test id ranges)
  (cond ((null? ranges) 0)
        ((in-range? id (car ranges)) 1)
        (else (test id (cdr ranges)))))

(define (in-range? id range)
  (if (and (not (< (string->number id)
                  (string->number (car range))))
           (not (> (string->number id)
                  (string->number (cadr range)))))
      #t
      #f))

(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(fresh-counter ids ranges)
