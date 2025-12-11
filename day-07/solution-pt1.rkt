#!/usr/bin/env racket
#lang racket/base

(define in (open-input-file "input.txt"))

(define (file-reader file)
  (let ((line (read-line file)))
    (if (eof-object? line)
        '()
        (append (list (string->list line)) (file-reader file)))))

(define input (file-reader in))

(define (beam-splitter lines prev count)
  (if (null? lines)
      count
      (let ((curr (car lines)))
        (let ((new-line (line-parser prev curr #f '() 0)))
          (beam-splitter (cdr lines)
                         (car new-line)
                         (+ count (cdr new-line)))))))

(define (line-parser prev curr close? res split-count)
  (cond ((null? curr)
         (cons (append res '()) split-count))
        ((and close? (null? (cdr curr)))
         (cons (append res (cons #\| '())) split-count))
        ((null? (cdr curr))
         (if (equal? (car prev) #\|)
             (cons (append res (cons #\| '())) split-count)
             (cons (append res (cons (car curr) '())) split-count)))
        ((and close? (equal? (cadr curr) #\.))
         (line-parser (cdr prev)
                      (cdr curr)
                      #f
                      (append res (list #\|))
                      split-count))
        ((and (equal? (cadr curr) #\^)
              (equal? (cadr prev) #\|))
         (line-parser (cddr prev)
                      (cddr curr)
                      #t
                      (append res (list #\| (cadr curr)))
                      (+ split-count 1)))
        ((equal? (car prev) #\|)
         (line-parser (cdr prev)
                      (cdr curr)
                      #f
                      (append res (list #\|))
                      split-count))
        (else (line-parser (cdr prev)
                           (cdr curr)
                           #f
                           (append res (list (car curr)))
                           split-count))))

(define (first-line-parser first-line second-line)
  (cond ((null? second-line) '())
        ((equal? (car first-line) #\S)
         (cons #\|
               (first-line-parser (cdr first-line)
                                  (cdr second-line))))
        (else (cons (car second-line)
                    (first-line-parser (cdr first-line)
                                       (cdr second-line))))))

(define first-prev (first-line-parser (car input) (cadr input)))

(beam-splitter (cddr input) first-prev 0)
