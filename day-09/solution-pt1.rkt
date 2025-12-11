#!/usr/bin/env racket
#lang racket/base

(require racket/string)

(define in (open-input-file "input.txt"))

(define (file-reader file stop proc res)
  (let ((new-line (read-line file)))
    (if (stop new-line)
        res
        (file-reader file stop proc (append res (proc new-line))))))

(define input
  (file-reader in
               eof-object?
               (lambda (x)
                 (list (map string->number
                            (string-split x ","))))
               '()))

(define (rect-finder curr-tile rest-tiles res)
  (if (null? rest-tiles)
      res
      (rect-finder
        (car rest-tiles)
        (cdr rest-tiles)
        (append res
                (flatmap (lambda (x)
                           (list (rect-size curr-tile x)))
                         rest-tiles)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define x-cor car)
(define y-cor cadr)

(define (rect-size tile1 tile2)
  (list (* (+ (abs (- (x-cor tile2) (x-cor tile1))) 1)
           (+ (abs (- (y-cor tile2) (y-cor tile1))) 1))
        tile1
        tile2))

(define (largest-rect input)
  (let ((rect-sizes
          (rect-finder (car input) (cdr input) '())))
    (let ((sorted
            (sort rect-sizes
                  (lambda (x y)
                    (> (car x) (car y))))))
      (let ((largest-coords (car sorted)))
        (car largest-coords)))))

(largest-rect input)
