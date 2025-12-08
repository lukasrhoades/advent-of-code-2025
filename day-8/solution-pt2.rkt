#!/usr/bin/env racket
#lang racket/base

(require racket/string)
(require racket/set)

(define in (open-input-file "input.txt"))

(define (file-reader file stop proc res)
  (let ((new-line (read-line file)))
    (if (stop new-line)
        res
        (file-reader file
                     stop
                     proc
                     (append res (proc new-line))))))

(define input
  (file-reader
    in
    eof-object?
    (lambda (x)
      (list (map string->number (string-split x ","))))
    '()))

(define (distance-matrix curr-node remaining-nodes)
  (if (null? remaining-nodes)
      '()
      (append (flatmap (lambda (x)
                         (list (distance curr-node x)))
                       remaining-nodes)
              (distance-matrix (car remaining-nodes)
                               (cdr remaining-nodes)))))

(define (distance node1 node2)
  (let ((x1 (car node1)) (x2 (car node2))
        (y1 (cadr node1)) (y2 (cadr node2))
        (z1 (caddr node1)) (z2 (caddr node2)))
    (let ((dist
            (sqrt (+ (square (- x2 x1))
                     (square (- y2 y1))
                     (square (- z2 z1))))))
      (list dist node1 node2))))

(define (square x) (* x x))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define num-juncbox (length input))
(define dist-matrix 
  (sort (distance-matrix (car input) (cdr input))
        (lambda (x y)
          (< (car x) (car y)))))

(define (dist-from-wall input)
  (circuit-builder input '() '()))

(define (circuit-builder input prev-pair circuits)
  (if (and (= (length circuits) 1)
           (= (length (car circuits)) num-juncbox))
      (* (caadr prev-pair) (caaddr prev-pair))
      (let ((curr-pair (car input)))
        (let ((juncbox1 (cadr curr-pair))
              (juncbox2 (caddr curr-pair)))
          (let ((new-circuits
                  (add-circuit juncbox1
                               juncbox2
                               circuits)))
            (circuit-builder (cdr input)
                             curr-pair
                             new-circuits))))))

(define (add-circuit jb1 jb2 circuits)
  (if (null? circuits)
      (append circuits (list (list jb1 jb2)))
      (cond ((and (set-member? (car circuits) jb1)
                  (set-member? (car circuits) jb2))
             circuits)
            ((set-member? (car circuits) jb1)
             (let ((merging (circuit-merger
                              jb2
                              (cdr circuits)
                              '()
                              '())))
               (append (cdr merging)
                       (list (append (car merging)
                                     (cons jb2 (car circuits)))))))
            ((set-member? (car circuits) jb2)
             (let ((merging (circuit-merger
                              jb1
                              (cdr circuits)
                              '()
                              '())))
               (append (cdr merging)
                       (list (append (car merging)
                                     (cons jb1 (car circuits)))))))
            (else
              (append (list (car circuits))
                      (add-circuit jb1 jb2 (cdr circuits)))))))

(define (circuit-merger jb circuits to-add res)
  (if (null? circuits)
      (cons to-add res)
      (cond ((set-member? (car circuits) jb)
             (circuit-merger
               jb
               (cdr circuits)
               (append to-add (set-subtract (car circuits)
                                            (list jb)))
               res))
            (else (circuit-merger
                    jb
                    (cdr circuits)
                    to-add
                    (append res (list (car circuits))))))))

(dist-from-wall dist-matrix)
