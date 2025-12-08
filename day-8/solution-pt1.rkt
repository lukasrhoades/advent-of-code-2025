#!/usr/bin/env racket
#lang racket/base

(require racket/string)
(require racket/list)
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

(define dist-matrix 
  (sort (distance-matrix (car input) (cdr input))
        (lambda (x y)
          (< (car x) (car y)))))

; connect the first 1000 pairs
(define proc-input (take dist-matrix 1000))

(define (size-circuits input)
  (product-circuit (circuit-builder input '())))

(define (circuit-builder input circuits)
  (if (null? input)
      circuits
      (let ((curr-pair (car input)))
        (let ((juncbox1 (cadr curr-pair))
              (juncbox2 (caddr curr-pair)))
          (let ((new-circuits
                  (add-circuit juncbox1
                               juncbox2
                               circuits)))
            (circuit-builder (cdr input)
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

; find product of 3 biggest circuits
(define (product-circuit circuits)
  (let ((sort-by-length
          (sort (map (lambda (x)
                       (cons (length x) x))
                     circuits)
                (lambda (x y)
                  (> (car x) (car y))))))
    (* (caar sort-by-length)
       (caadr sort-by-length)
       (caaddr sort-by-length))))

(size-circuits proc-input)
