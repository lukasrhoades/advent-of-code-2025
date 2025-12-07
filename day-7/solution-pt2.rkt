#!/usr/bin/env racket
#lang racket/base

(require racket/port)
(require racket/list)

(define in (open-input-file "input.txt"))

(define (file-reader file)
  (let ((line (read-line file)))
    (if (eof-object? line)
        '()
        (append (list (string->list line)) (file-reader file)))))

(define input (file-reader in))

(define (world-counter prev lines)
  (if (null? lines)
      (sum prev)
      (let ((curr (car lines)))
        (let ((new-line (quantum-parser prev curr '())))
          (world-counter new-line (cdr lines))))))

(define (quantum-parser prev curr res)
  (if (null? curr)
      res
      (let ((number-worlds (car prev)))
        (if (equal? (car curr) #\^)
            (quantum-parser
              (cddr prev)
              (cddr curr)
              (append
                (append
                  (take res (- (length res) 1))
                  (list (+ (list-ref res (- (length res) 1))
                           number-worlds)))
                (list 0 (+ number-worlds
                           (cadr prev)))))
            (quantum-parser (cdr prev)
                            (cdr curr)
                            (append res (list number-worlds)))))))

(define (first-line-parser first-line second-line)
  (cond ((null? second-line) '())
        ((equal? (car first-line) #\S)
         (cons #\|
               (first-line-parser (cdr first-line)
                                  (cdr second-line))))
        (else (cons (car second-line)
                    (first-line-parser (cdr first-line)
                                       (cdr second-line))))))

(define (first-prev-converter first-prev)
  (map (lambda (x)
         (if (equal? x #\|) 1 0))
       first-prev))

(define first-prev
  (first-prev-converter
    (first-line-parser (car input) (cadr input))))

(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(world-counter first-prev (cddr input))
