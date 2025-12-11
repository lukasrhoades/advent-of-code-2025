#!/usr/bin/env racket
#lang racket/base

(require racket/string)

(define in (open-input-file "input.txt"))

(define (rotator input currpos count)
  (if (eof-object? input)
      count
      (let ((split (string-split input #px"(?<=\\D)")))
        (let ((direction (car split))
              (distance (string->number (cadr split))))
          (let ((dirop (if (equal? direction "L") - +)))
            (let ((unmod (dirop currpos distance)))
              (let ((newpos (dial unmod 100))
                    (point0 (times unmod)))
                (if (and (= currpos 0) (eq? dirop -))
                    (rotator (read-line in) newpos (+ count point0 (- 1)))
                    (rotator (read-line in) newpos (+ count point0))))))))))

(define (dial num modulus)
  (if (< num 0)
      (remainder (+ modulus (remainder num modulus)) modulus)
      (remainder num modulus)))

(define (times unmod)
  (cond ((> unmod 99) (floor (/ unmod 100)))
        ((< unmod 1) (floor (/ (+ (abs unmod) 100) 100)))
        (else 0)))

;(times (+ 0 50))   ; 0
;(times (- 0 50))   ; 1, but don't count it

;(times (+ 50 49))  ; 0
;(times (+ 50 50))  ; 1 
;(times (+ 50 149)) ; 1
;(times (+ 50 150)) ; 2
;(times (- 50 49))  ; 0
;(times (- 50 50))  ; 1
;(times (- 50 149)) ; 1
;(times (- 50 150)) ; 2

(rotator (read-line in) 50 0)
