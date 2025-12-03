#!/usr/bin/env racket
#lang racket/base

(require racket/port)
(require rebellion/base/converter)

(define in (open-input-file "input.txt"))

(define (sum-volts input count)
  (if (eof-object? input)
      count
      (sum-volts
        (read-line in)
        (+ count (bank-voltage (split-line input))))))

(define (bank-voltage bank)
  (let ((first-digit (highest-number bank 1)))
    (let ((second-digit (highest-number (cdr first-digit) 2)))
      (+ (* (car first-digit) 10) (car second-digit)))))

(define (highest-number line pass)
  (define (iter curr high seq)
    (if (or (and (= pass 1) (null? (cdr curr)))
            (and (= pass 2) (null? curr)))
        (cons high seq)
        (let ((a (string->number (car curr))))
          (if (> a high)
              (iter (cdr curr) a (cdr curr))
              (iter (cdr curr) high seq)))))
  (iter line 0 line))

(define (split-line line)
  (regexp-match* #px"\\d{1}" line))

(sum-volts (read-line in) 0)
