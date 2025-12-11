#!/usr/bin/env racket
#lang racket/base

(require racket/string)
(require racket/list)

(define in (open-input-file "input.txt"))

(define (file-reader file stop proc res)
  (let ((new-line (read-line file)))
    (if (stop new-line)
        res
        (file-reader file stop proc (append res (proc new-line))))))

(define input
  (file-reader in
               eof-object?
               (lambda (x)
                 (list (map string->number
                            (string-split x ","))))
               '()))

(define (rect-inside-boundary? tile1 tile2 prev-bound boundary)
  ; checks for intersections of rect with a given edge
  (if (null? boundary)
      #t
      (let ((lrect (min (x-cor tile1) (x-cor tile2)))
            (urect (max (y-cor tile1) (y-cor tile2)))
            (rrect (max (x-cor tile1) (x-cor tile2)))
            (drect (min (y-cor tile1) (y-cor tile2)))
            (b1 prev-bound)
            (b2 (car boundary)))
        (let ((lbound (min (x-cor b1) (x-cor b2)))
              (ubound (max (y-cor b1) (y-cor b2)))
              (rbound (max (x-cor b1) (x-cor b2)))
              (dbound (min (y-cor b1) (y-cor b2))))
          (if (= lbound rbound)
              (let ((vert lbound))
                (if (and (and (> vert lrect)
                              (< vert rrect))
                         (or (and (>= ubound urect)
                                  (< dbound urect))
                             (and (> ubound drect)
                                  (<= dbound drect))))
                    #f
                    (rect-inside-boundary? tile1
                                           tile2
                                           b2
                                           (cdr boundary))))
              (let ((horiz ubound))
                (if (and (and (> horiz drect)
                              (< horiz urect))
                         (or (and (<= lbound lrect)
                                  (> rbound lrect))
                             (and (< lbound rrect)
                                  (>= rbound rrect))))
                    #f
                    (rect-inside-boundary? tile1
                                           tile2
                                           b2
                                           (cdr boundary)))))))))
(define x-cor car)
(define y-cor cadr)

(define (rect-finder curr-tile rest-tiles boundary res)
  (if (null? rest-tiles)
      res
      (rect-finder
        (car rest-tiles)
        (cdr rest-tiles)
        boundary
        (append res
                (flatmap (lambda (x)
                           (list (rect-size curr-tile x boundary)))
                         rest-tiles)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (rect-size tile1 tile2 boundary)
  (if (rect-inside-boundary? tile1
                             tile2
                             (car boundary)
                             (append (cdr boundary)
                                     (list (car boundary)))) ; last edge
      (list (* (+ (abs (- (x-cor tile2) (x-cor tile1))) 1)
               (+ (abs (- (y-cor tile2) (y-cor tile1))) 1))
            tile1
            tile2)
      (list 0 tile1 tile2)))

(define (largest-rect input)
    (let ((rect-sizes
            (rect-finder (car input) (cdr input) input '())))
      (car (argmax car rect-sizes))))

(largest-rect input)
