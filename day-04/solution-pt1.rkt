#!/usr/bin/env racket
#lang racket/base

(define in (open-input-file "input.txt"))

(define (grid-search prev curr next res)
  (let ((next (read-line in)))
    (if (eof-object? next)
        (+ res (check-adjacent 0 prev curr last-line 0))
        (let ((next-list (regexp-match* #px"\\S{1}" next)))
          (grid-search curr
                       next-list
                       0
                       (+ res (check-adjacent
                              0
                              prev
                              curr
                              next-list
                              0)))))))

(define (check-adjacent i prev curr-row next sum)
  (let ((len (length curr-row)))
    (if (= i len)
        sum
        (let ((curr-pos (index i curr-row)))
          (cond ((equal? curr-pos ".")
                 (check-adjacent (+ i 1) prev curr-row next sum))
                (else
                  (let ((tl (index (- i 1) prev))
                        (tt (index i prev))
                        (tr (index (+ i 1) prev))
                        (ml (index (- i 1) curr-row))
                        (mr (index (+ i 1) curr-row))
                        (bl (index (- i 1) next))
                        (bb (index i next))
                        (br (index (+ i 1) next)))
                    (let ((test (paper-test
                                  (list tl tt tr ml mr bl bb br)
                                  0)))
                      (check-adjacent (+ i 1)
                                      prev
                                      curr-row
                                      next
                                      (+ sum test))))))))))

(define (paper-test positions count)
  (cond ((> count 3) 0)
        ((null? positions) 1)
        ((equal? (car positions) "@")
         (paper-test (cdr positions) (+ count 1)))
        ((equal? (car positions) ".")
         (paper-test (cdr positions) count))))

(define (index i list)
  (cond ((or (< i 0) (= i (length list))) ".")
        ((= i 0) (car list))
        (else (index (- i 1) (cdr list)))))

(define first-line (regexp-match* #px"\\S{1}" (read-line in)))
(define len (length first-line))
(define (builder len char)
  (if (= len 0)
      '()
      (cons char (builder (- len 1) char))))
(define prev-line (builder len "."))
(define last-line prev-line)

(grid-search prev-line first-line 0 0)
