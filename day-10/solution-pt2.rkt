#!/usr/bin/env racket
#lang racket/base

(require glpk)
(require racket/string)
(require racket/list)
(require megaparsack megaparsack/text)

(define in (open-input-file "input.txt"))

(define (read-file file stop proc res)
  (let ((new-line (read-line file)))
    (if (stop new-line)
        res
        (read-file file stop proc (append res (proc new-line))))))

(define regex #px"(\\[.*\\]){1}\\s{1}(.+){1}\\s{1}(\\{{1}.*\\}){1}")

(define input
  (read-file
    in
    eof-object?
    (lambda (x)
      (list
        (append
          (list
            (map
              (lambda (x)
                (parse-result!
                  (parse-string
                    (many/p integer/p #:sep (char/p #\,))
                    (string-trim x #px"[\\(\\)]"))))
              (string-split
                (car (regexp-match* regex x #:match-select third))
                     " ")))
          (map
            (lambda (x)
              (parse-result!
                (parse-string
                  (many/p integer/p #:sep (char/p #\,))
                  (string-trim x #px"[\\{\\}]"))))
            (string-split
              (car (regexp-match* regex x #:match-select fourth))
                   " ")))))
    '()))

(define (total-buttons input)
  (inexact->exact (sum (map fewest-presses input))))
(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (fewest-presses input)
  (let ((buttons (car input))
        (joltages (cadr input)))
    (caaddr
      (mip-solve
        (objective-builder 1 (length buttons) '(0))
        'min
        (constraint-builder
          buttons
          1
          (vector-builder 1 (length joltages) '()))
        (bounds-builder joltages buttons)
        (int-vars-builder buttons)))))

(define (list-builder start end proc res)
  (if (> start end)
      res
      (list-builder
        (+ start 1)
        end
        proc
        (append res
                (proc start)))))

(define (btn-var-builder num)
  (list
    1
    (string->symbol
      (string-append "x"
                     (number->string num)))))

(define (objective-builder start end res)
  (list-builder
    start
    end
    (lambda (x)
      (list (btn-var-builder x)))
    res))

(define (jolt-var-builder num)
  (list
    (string->symbol
      (make-string
        1
        (string-ref "abcdefghijklmonpqrs" (- num 1))))))

(define (vector-builder start end res)
  (list-builder
    start
    end
    (lambda (x)
      (list (jolt-var-builder x)))
    res))

(define (constraint-builder buttons bnum joltage-vector)
  (define (iter button bnum joltage-vector)
    (if (null? button)
        joltage-vector
        (let ((index (car button)))
          (let ((to-modify
                  (get-val-at-index index joltage-vector)))
            (let ((new-list
                    (append to-modify
                            (list (btn-var-builder bnum)))))
              (iter
                (cdr button)
                bnum
                (list-set joltage-vector index new-list)))))))
  (if (null? buttons)
      joltage-vector
      (let ((curr-button (car buttons)))
        (constraint-builder
          (cdr buttons)
          (+ bnum 1)
          (iter curr-button bnum joltage-vector)))))

(define (get-val-at-index i lst)
  (if (= i 0)
      (car lst)
      (get-val-at-index (- i 1) (cdr lst))))

(define (bounds-builder joltages buttons)
  (define (jolt-bound-iter joltages jnum res)
    (if (null? joltages)
        res
        (jolt-bound-iter (cdr joltages)
                         (+ jnum 1)
                         (append res
                                 (list
                                   (list
                                     (car (jolt-var-builder jnum))
                                     (car joltages)
                                     (car joltages)))))))
  (define (btn-bound-iter buttons bnum res)
    (if (null? buttons)
        res
        (btn-bound-iter (cdr buttons)
                        (+ bnum 1)
                        (append res
                                (list
                                  (list
                                    (cadr (btn-var-builder bnum))
                                    0
                                    `posinf))))))
  (append (jolt-bound-iter joltages 1 '())
          (btn-bound-iter buttons 1 '())))

(define (int-vars-builder buttons)
  (define (iter buttons bnum res)
    (if (null? buttons)
        res
        (iter (cdr buttons)
              (+ bnum 1)
              (append res
                      (list (cadr (btn-var-builder bnum)))))))
  (iter buttons 1 '()))

(total-buttons input)
