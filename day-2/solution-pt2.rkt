#!/usr/bin/env racket
#lang racket/base

(require racket/port)
(require racket/string)
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
  (if (id-invalid? id)
      (string->number id)
      0))

(define (id-invalid? id)
  (define (test-substring id n)
    (let ((sub (substring id 0 n))
          (px (pregexp (string-append "\\d{"
                                     (number->string n)
                                     "}"))))
      (let ((to-test (regexp-match* px id)))
        (if (< (length to-test) (/ (string-length id) n))
            0
            (let ((sum (accumulate
                         (lambda (x y)
                           (+ (if (equal? sub x) 1 0) y))
                         0
                         to-test)))
              (if (< sum (length to-test))
                  0
                  1))))))
  (define (test-id start end res)
    (if (> res 0)
        #t
        (if (> start end)
            #f
            (test-id (+ start 1)
                     end
                     (+ (test-substring id start) res)))))
  (let ((end (/ (string-length id) 2)))
    (test-id 1 end 0)))

(invalid-add ranges 0)
