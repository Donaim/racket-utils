
#lang racket

(require "racket-utils.rkt")

((* _ 2) 2)

(letin-with-full dom-print
                 [a (+ 2 7)]
                 [b (* a 10)]
                 (* b b))

(define [calc-default]
  (dom (dom-default (= _ 0))
       [a (+ 2 7)]
       [b (* a 10)]
       [c (- b b)]
       (+ 100 c)))

(printf "dom-default = ~a\n" (calc-default))

(printf "dom parameterized = ~a\n"
        (letin-parameterize
         (fn f fname
             (fn xqt resultqt x cont
                 (add1 (f xqt resultqt x cont))))
         (calc-default)))

