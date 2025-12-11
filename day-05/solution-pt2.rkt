#!/usr/bin/env racket
#lang racket/base

(require racket/string)

(define in (open-input-file "input.txt"))

(define (file-reader file res proc stop)
  (let ((line (read-line file)))
    (if (stop line)
        res
        (file-reader file (append res (proc line)) proc stop))))

(define ranges
  (file-reader in
               '()
               (lambda (x)
                 (list (map string->number (string-split x "-"))))
               (lambda (x) (equal? x ""))))

(define (sum-ranges ranges)
  (sum (map length-range (merge-ranges (sort-start ranges)))))

(define (sort-start ranges)
  (sort ranges (lambda (a b) (< (car a) (car b)))))

(define make-range list)
(define start car)
(define end cadr)

(define (length-range range)
  (+ 1 (- (end range) (start range))))

(define (sum seq) (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (merge-ranges ranges)
  (let ((prev (length ranges)))
    (let ((res (range-merger ranges)))
      (let ((new (length res)))
        (if (= prev new)
            res
            (merge-ranges res))))))

(define (range-merger ranges)
  (cond ((null? ranges) '())
        ((null? (cdr ranges)) ranges)
        (else (let ((start1 (start (car ranges)))
                    (end1 (end (car ranges)))
                    (start2 (start (cadr ranges)))
                    (end2 (end (cadr ranges))))
                (cond ((and (>= start1 start2) (<= end1 end2))
                       (range-merger (cdr ranges)))
                      ((and (>= start2 start1) (<= end2 end1))
                       (range-merger (cons (car ranges)
                                           (cddr ranges))))
                      ((and (>= end1 start2) (<= start1 start2))
                       (let ((new-range (make-range start1 end2)))
                         (cons new-range
                               (range-merger (cons new-range
                                                   (cddr ranges))))))
                      ((and (>= end2 start1) (<= start2 start1))
                       (let ((new-range (make-range start2 end1)))
                         (cons new-range
                               (range-merger (cons new-range
                                                   (cddr ranges))))))
                      (else (cons (car ranges)
                                  (range-merger (cdr ranges)))))))))

(sum-ranges ranges)
