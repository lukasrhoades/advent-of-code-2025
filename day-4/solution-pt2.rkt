#!/usr/bin/env racket
#lang racket/base

(require racket/port)

(define in (open-input-file "input.txt"))

(define (file-reader file res)
  (let ((line (read-line file)))
    (if (eof-object? line)
        res
        (file-reader file (append res (list line))))))

(define (multi-grid grid total-sum)
  (let ((new-gs (grid-search prev-line
                             (regexp-match* #px"\\S{1}"
                                            (car grid))
                             0
                             0
                             (cdr grid)
                             '())))
    (let ((new-sum (car new-gs)) (new-grid (cdr new-gs)))
      (if (= new-sum 0)
          total-sum
          (multi-grid new-grid (+ total-sum new-sum))))))

(define (grid-search prev curr next res curr-grid new-grid)
  (if (null? curr-grid)
      (let ((last-row-check
              (check-adjacent 0 prev curr last-line "" 0)))
        (cons (+ res (car last-row-check))
              (append new-grid (list (cdr last-row-check)))))
      (let ((next (car curr-grid)))
        (let ((next-list (regexp-match* #px"\\S{1}" next)))
          (let ((row-check (check-adjacent 0
                                           prev
                                           curr
                                           next-list
                                           ""
                                           0)))
            (grid-search curr
                         next-list
                         0
                         (+ res (car row-check))
                         (cdr curr-grid)
                         (append new-grid (list (cdr row-check)))))))))

(define (check-adjacent i prev curr-row next new-row sum)
  (let ((len (length curr-row)))
    (cond ((= i len) (cons sum new-row))
          (else (let ((curr-pos (index i curr-row)))
                  (cond ((equal? curr-pos ".")
                         (check-adjacent (+ i 1)
                                         prev
                                         curr-row
                                         next
                                         (string-append
                                           new-row
                                           curr-pos)
                                         sum))
                        (else (let ((tl (index (- i 1) prev))
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
                                                  (string-append
                                                    new-row
                                                    (which-char
                                                      test
                                                      curr-pos))
                                                  (+ sum test)))))))))))

(define (which-char test char)
  (if (= test 1)
      "."
      char))

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

(define first-line (read-line in))
(define list-fl (regexp-match* #px"\\S{1}" first-line))
(define len (length list-fl))
(define (builder len char)
  (if (= len 0)
      '()
      (cons char (builder (- len 1) char))))
(define prev-line (builder len "."))
(define last-line prev-line)

(define initial-grid (file-reader in (list first-line)))

(multi-grid initial-grid 0)
