#!/usr/bin/env racket
#lang racket/base

(require racket/string)

(define in (open-input-file "input.txt"))

(define (file-reader file stop proc res)
  (let ((new-line (read-line file)))
    (cond ((stop new-line) res)
          (else (proc new-line)
                (file-reader file stop proc res)))))

(define graph (make-hash))

(define input
  (file-reader in
               eof-object?
               (lambda (x)
                 (hash-set! graph
                            (cadr
                              (regexp-match #px"\\b(\\D{3}):" x))
                            (string-split
                              (string-trim x #px"\\b\\D{3}:")
                              " ")))
               graph))

(define (pathfind curr-node visit-dac? visit-fft?)
  (cond ((and (equal? curr-node "out")
              visit-dac?
              visit-fft?) 1)
        ((equal? curr-node "out") 0)
        ((equal? curr-node "dac")
         (sum (map (lambda (x)
                     (try-cache
                       (list pathfind x #t visit-fft?)))
                   (get-neighbors curr-node))))
        ((equal? curr-node "fft")
         (sum (map (lambda (x)
                     (try-cache
                       (list pathfind x visit-dac? #t)))
                   (get-neighbors curr-node))))
        (else (sum (map (lambda (x)
                          (try-cache
                            (list pathfind x visit-dac? visit-fft?)))
                        (get-neighbors curr-node))))))
(define (sum seq)
  (accumulate + 0 seq))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define cache (make-hash))

(define (try-cache procedure-call)
  (let ((miss? (hash-ref cache procedure-call "miss")))
    (cond ((equal? miss? "miss")
           (let ((result
                   ((car procedure-call)
                    (cadr procedure-call)
                    (caddr procedure-call)
                    (cadddr procedure-call))))
             (hash-set! cache procedure-call result)
             result))
          (else miss?))))

(define (get-neighbors key)
  (hash-ref graph key))

(pathfind "svr" #f #f)
