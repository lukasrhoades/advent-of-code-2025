#!/usr/bin/env racket
#lang racket/base

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
  (read-file in
             eof-object?
             (lambda (x)
               (list
                 (list
                   (string-trim
                     (regexp-replace*
                       #px"\\#"
                       (regexp-replace*
                         #px"\\."
                         (car (regexp-match* regex x #:match-select second))
                         "0")
                       "1")
                     #px"[\\[\\]]")
                   (map
                     (lambda (x)
                       (parse-result!
                         (parse-string
                           (many/p integer/p #:sep (char/p #\,))
                           (string-trim x #px"[\\(\\)]"))))
                     (string-split
                       (car (regexp-match* regex x #:match-select third))
                            " ")))))
             '()))

(define (total-buttons input)
  (sum (map fewest-presses input)))
(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (fewest-presses machine)
  (define (iter-subsets target initial subsets)
    (let ((subset (car subsets)))
      (let ((result-subset
              (iter-subset target initial (length subset) subset)))
        (if (> result-subset 0)
            result-subset
            (iter-subsets target initial (cdr subsets))))))
  (define (iter-subset target initial num-buttons subset)
    (if (null? subset)
        (if (equal? initial target)
            num-buttons
            0)
        (let ((curr-button (car subset)))
          (iter-subset target
                       (apply-button initial curr-button)
                       num-buttons
                       (cdr subset)))))
  (define (apply-button initial button)
    (let ((curr (string-copy initial)))
      (if (null? button)
          curr
          (let ((curr-toggle (car button)))
            (if (equal? (string-ref curr curr-toggle) #\0)
                (string-set! curr curr-toggle #\1)
                (string-set! curr curr-toggle #\0))
            (apply-button curr (cdr button))))))
  (let ((target (car machine)))
    (let ((initial (make-string (string-length target) #\0))
          (subsets
            (sort (combinations (cadr machine))
                  (lambda (x y)
                    (< (length x) (length y))))))
      (iter-subsets target initial subsets))))

(total-buttons input)
