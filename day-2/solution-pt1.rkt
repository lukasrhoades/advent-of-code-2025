#!/usr/bin/env racket
#lang racket/base

(require racket/port)
(require racket/string)
(require megaparsack megaparsack/text)
(require rebellion/base/converter)

(define in (open-input-file "input.txt"))

(define ranges (string-split (read-line in) #rx","))

(define (invalid-add ranges res)
  (if (null? ranges)
      res
      (invalid-add
        (cdr ranges)
        (+ (accumulate + 0 (map id-tester
                                (range-extractor (car ranges))))
           res))))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))
(define (map proc seq)
  (accumulate (lambda (x y) (cons (proc x) y)) '() seq))

(define (range-extractor range)
  (let ((endpoints (string-split range #rx"-")))
    (let ((start (car endpoints)) (end (cadr endpoints)))
      (define (id-list-builder start end)
        (if (> (string->number start) (string->number end))
            '()
            (let ((next
                    (number->string
                      (+ (string->number start) 1))))
              (cons start
                    (id-list-builder next end)))))
      (id-list-builder start end))))
                  
(define (id-tester id)
  (let ((len (string-length id)))
    (if (odd? len)
        0
        (let ((half (/ len 2)))
          (let ((a (substring id 0 half))
                (b (substring id half)))
            (if (equal? a b)
                (parse-result! (parse-string integer/p id))
                0))))))
(define (odd? x)
  (not (= (remainder x 2) 0)))

(invalid-add ranges 0)
