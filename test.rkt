
#lang racket

(require "racket-utils.rkt")

((* _ 2) 2)

(letin-with-full dom-print
     [a (+ 2 7)]
     [b (* a 10)]
     (* b b))

(printf "dom-default = ~a\n"
        (dom (dom-default (= _ 0))
             [a (+ 2 7)]
             [b (* a 10)]
             [c (- b b)]
             (+ 100 c)))

