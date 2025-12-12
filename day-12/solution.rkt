#!/usr/bin/env racket
#lang racket/base

(require racket/string)

(define in (open-input-file "input.txt"))

(define (file-reader file stop proc res)
  (let ((new-line (read-line file)))
    (if (stop new-line)
        res
        (file-reader file stop proc (append res (proc new-line))))))

(define px #px"(\\d{2}x\\d{2}):\\s{1}(.*)")

(define regions
  (file-reader in
               eof-object?
               (lambda (x)
                 (if (regexp-match? px x)
                     (let ((rx-match (regexp-match px x)))
                       (list
                         (list
                           (map string->number
                                (string-split (cadr rx-match) "x"))
                           (map string->number
                                (string-split (caddr rx-match) " ")))))
                     '()))
               '()))

(define (sum-regions regions)
  (sum (map fit-test regions)))
(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (fit-test region)
  (let ((dimensions (car region))
        (present-req (cadr region)))
    (let ((max-area (* (car dimensions) (cadr dimensions))))
      (if (<= (* 9 (sum present-req)) max-area)
          1
          0))))

(sum-regions regions)
