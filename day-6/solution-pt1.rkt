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
               (lambda (x) (list (regexp-split #px"\\s+"
                                               (string-trim x))))
               '()))

(define (cephalopod-mather input)
  (sum (procedure-applier (procedure-maker input))))

(define (procedure-maker lists)
  (let ((operators (map (lambda (x)
                          (if (equal? x "*") * +))
                        (car (cddddr lists))))
        (operands1 (map string->number (car lists)))
        (operands2 (map string->number (cadr lists)))
        (operands3 (map string->number (caddr lists)))
        (operands4 (map string->number (cadddr lists))))
    (map list operators operands1 operands2 operands3 operands4)))

(define (procedure-applier procedure-lists)
  (map (lambda (x)
         (let ((operator (car x))
               (operand1 (cadr x))
               (operand2 (caddr x))
               (operand3 (cadddr x))
               (operand4 (car (cddddr x))))
           (operator operand1 operand2 operand3 operand4)))
       procedure-lists))

(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(cephalopod-mather input)
