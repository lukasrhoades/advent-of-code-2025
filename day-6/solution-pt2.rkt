#!/usr/bin/env racket
#lang racket/base

(require racket/port)
(require racket/string)

(define in (open-input-file "input.txt"))

(define (file-reader file stop proc res)
  (let ((line (read-line file)))
    (if (stop line)
        res
        (file-reader file stop proc (append res (proc line))))))

(define input
  (file-reader in
               eof-object?
               list
               '()))

(define (cephalopod-mather input)
  (sum (procedure-applier
         (whitespace-parser input))))

(define (whitespace-parser input)
  (define (parser operators list1 list2 list3 list4 one-op all-ops)
    (if (not (non-empty-string? list1))
        (append (list (cons (car operators) one-op))
                all-ops)
        (let ((x1 (string-ref list1 0))
              (x2 (string-ref list2 0))
              (x3 (string-ref list3 0))
              (x4 (string-ref list4 0)))
          (if (and (equal? x1 #\space)
                   (equal? x2 #\space)
                   (equal? x3 #\space)
                   (equal? x4 #\space))
              (parser (cdr operators)
                      (substring list1 1)
                      (substring list2 1)
                      (substring list3 1)
                      (substring list4 1)
                      '()
                      (append (list (cons (car operators) one-op))
                              all-ops))
              (parser operators
                      (substring list1 1)
                      (substring list2 1)
                      (substring list3 1)
                      (substring list4 1)
                      (cons (string->number
                              (string-trim
                                (string x1 x2 x3 x4)))
                            one-op)
                      all-ops)))))
  (let ((operators (map (lambda (x)
                          (if (equal? x "*") * +))
                        (regexp-split
                          #px"\\s+"
                          (string-trim (car (cddddr input))))))
        (operands1 (car input))
        (operands2 (cadr input))
        (operands3 (caddr input))
        (operands4 (cadddr input)))
    (parser operators operands1 operands2 operands3 operands4 '() '())))

(define (procedure-applier procedure-lists)
  (map (lambda (x)
         (let ((operator (car x))
               (operands (cdr x)))
           (apply operator operands)))
       procedure-lists))

(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(cephalopod-mather input)
