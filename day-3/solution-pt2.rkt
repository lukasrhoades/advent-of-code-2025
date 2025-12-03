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
  (define (iter digit seq res)
    (if (> digit 11)
        res
        (let ((highest (highest-number seq digit)))
          (iter (+ digit 1)
                (cdr highest)
                (+ res (* (expt 10 (- 11 digit))
                          (car highest)))))))
  (iter 0 bank 0))

(define (highest-number line pass)
  (let ((limit (- (length line) (- 12 pass))))
    (define (iter count curr high seq)
      (if (> count limit)
          (cons high seq)
          (let ((a (string->number (car curr))))
            (if (> a high)
                (iter (+ count 1) (cdr curr) a (cdr curr))
                (iter (+ count 1) (cdr curr) high seq)))))
    (iter 0 line 0 line)))

(define (split-line line)
  (regexp-match* #px"\\d{1}" line))

(sum-volts (read-line in) 0)
