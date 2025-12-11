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
            (let ((newpos (dial (dirop currpos distance) 100)))
              (if (= 0 newpos)
                  (rotator (read-line in) newpos (+ count 1))
                  (rotator (read-line in) newpos count))))))))

(define (dial num modulus)
  (if (< num 0)
      (remainder
        (+ modulus                  ; normalize 1 to 100
           (remainder num modulus)) ; normalize -99 to 0
         modulus)                   ; normalize 0 to 99
      (remainder num modulus)))      

;(dial 300 99) ; 0

;(dial -1 99)   ; 99
;(dial -99 99)  ; 1
;(dial -100 99) ; 0

(rotator (read-line in) 50 0)
