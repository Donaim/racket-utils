#lang racket

(require "fancy-app.rkt")

(require syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(provide #%app) ;; from fancy-app
(provide ..)    ;; from fancy-app
(provide (all-defined-out))

(define-syntax letin
  (syntax-rules ()
    [(letin body) body]
    [(letin ((a . as) b) body ...)
     (let-values [[[a . as] b]] (letin body ...))]
    [(letin (a b) body ...)
     (let [[a b]] (letin body ...))]))

(define-simple-macro [defloop args . body]
  #:with loop-name (format-id (syntax args) "loop")
  (letrec [[loop-name (lambda args . body)]] loop-name))

(define-syntax-rule [apploop args argv . body]
  ((defloop args . body) . argv))

(define-syntax-rule [defrec [id . args] . body]
  (letrec [[id (lambda args . body)]] id))

(define-syntax-rule [apprec id args argv . body]
  ((defrec [id . args] . body) . argv))

(define-syntax [reversed-args stx]
  (syntax-parse stx
    [(_ x ...) (define xs (syntax-e #'(x ...)))
               (define rev-xs (reverse xs))
               (datum->syntax stx (cons list rev-xs))]))

(define-syntax [f-reversed-args stx]
  (syntax-parse stx
    [(_ f x ...) (define xs (syntax-e #'(x ...)))
                 (define ff (syntax-e #'f))
                 (define rev-xs (reverse xs))
                 (datum->syntax stx (cons ff rev-xs))]))

(define-syntax reversed-lambda
  (syntax-rules ()
    [(reversed-lambda body args) (lambda [] body)]
    [(reversed-lambda body args next) (lambda (next . args) body)]
    [(reversed-lambda body args x next ...) (reversed-lambda body (x . args) next ...)]))

(define-syntax lm-start
  (syntax-rules ()
    [(lm-start args body) (reversed-lambda body () . args)]
    [(lm-start args x body ...) (lm-start (x . args) body ...)]))

(define-syntax-rule [lm . argv] (lm-start () . argv))

(define-syntax lm-list-g
  (syntax-rules ()
    [(lm-list-g lst body) body]
    [(lm-list-g lst x body ...)
     (let [[x (car lst)]]
       (lm-list-g
        (cdr lst)
        body
        ...))]))

(define-syntax-rule [lm-list . args]
  (lambda [lst] (lm-list-g lst . args)))

(define-syntax pairs
  (syntax-rules ()
    [(pairs (a b)) (list (cons a b))]
    [(pairs (a b) x ...) (cons
                          (cons a b)
                          (pairs x ...))]))

(define [monoid-f* op x . args]
  (if (null? args)
      x
      (apply monoid-f* (list* op (op x (car args)) (cdr args)))))

(define-syntax monoid-r*
  (syntax-rules ()
    [(monoid-r* op a) a]
    [(monoid-r* op a b ...)
     (op a (monoid-r* op b ...))]))

(define-syntax monoid*
  (syntax-rules ()
    [(monoid* op a) a]
    [(monoid* op a b c ...)
     (monoid* op (op a b) c ...)]))

(define [check-list-contract check-list args]
  (or (not check-list)
      (and (= (length check-list) (length args))
           (andmap (lambda [p x] (p x)) check-list args))))

(define-simple-macro [gfunc/define name]
  #:with add-name (format-id #'name "gfunc/instantiate-~a" (syntax-e #'name))
  #:with param-name (format-id #'name "gfunc/parameterize-~a" (syntax-e #'name))
  (define-values [name add-name param-name]
    (let [[internal-list (make-parameter null)]
          [sem (make-semaphore 1)]]
      (values
       (lambda args
         (let [[m (findf (lambda [p] (check-list-contract (car p) args)) (internal-list))]]
           (if m
               (apply (cdr m) args)
               (error (format "No gfunc instance of ~a accepts arguments ~a" name args)))))
       (lambda [args func]
         (semaphore-wait sem)
         (set! internal-list (make-parameter (append (internal-list) (list (cons args func)))))
         (semaphore-post sem))
       (lambda [args func body]
         (let [[new-list (cons (cons args func) (internal-list))]]
           (parameterize [[internal-list new-list]]
             (body))))))))

(define-simple-macro [gfunc/parameterize name check-list func . body]
  #:with param-name (format-id #'name "gfunc/parameterize-~a" (syntax-e #'name))
  (param-name check-list func (lambda [] . body)))

(define-simple-macro [gfunc/instance name check-list . body]
  #:with add-name (format-id #'name "gfunc/instantiate-~a" (syntax-e #'name))
  (add-name (list . check-list) (lambda [] . body)))

(define [read-file readf filepath]
  (letin
   [file (open-input-file filepath #:mode 'binary)]
   (do (file-position file eof))
   [len (file-position file)]
   (do (file-position file 0))
   [out (readf len file)]
   (do (close-input-port file))
   out))

(define [write-file writef filepath content]
  (define file (open-output-file filepath
                                 #:mode 'binary
                                 #:exists 'truncate/replace))
  (writef content file)
  (close-output-port file))

(define [read-file-string filepath] (read-file read-string filepath))
(define [write-file-string filepath content] (write-file write-string filepath content))
(define [read-file-bytes filepath] (bytes->list (read-file read-bytes filepath)))
(define [write-file-bytes filepath content] (write-file (lambda [x filepath] (write-bytes (list->bytes x) filepath)) filepath content))

(define [list-split skip? predicate lst]
  (apprec loop [buf lst i] [(list) lst 0]
    (if (null? lst)
        (if (null? buf) buf (list (reverse buf)))
        (letin
         [head (car lst)]
         [tail (cdr lst)]
         [newbuf (cons head buf)]
         (if (predicate head i)
             (cons (reverse (if skip? buf newbuf)) (loop '() tail (add1 i)))
             (loop newbuf tail (add1 i)))))))

(define [list-split-by count lst]
  (list-split #f (lambda [x i] (= (sub1 count) (modulo i count))) lst))

(define [flatten-1 lst]
  (foldl append null lst))

(define [replicate x n]
  (if (= 0 n)
      '()
      (cons x (replicate x (sub1 n)))))

(define [repeat-f n f x]
  (if (= n 0)
      x
      (repeat-f (sub1 n) f (f x))))

(define [list-base-n->integer base bts]
  (apploop [p left] [1 bts]
           (if (null? left) 0
               (+ (* p (car left)) (loop (* p base) (cdr left))))))

(define [bytes->integer bts] (list-base-n->integer 256))

(define/contract [maximize cmp f xs]
  (-> procedure? procedure? (and/c (not/c null?) (listof real?)) any/c)
  (let ([init-min-var (f (car xs))])
    (let loop ([min (car xs)]
               [min-var init-min-var]
               [xs (cdr xs)])
      (if (null? xs)
          min
          (let ([new-min (f (car xs))])
            (if (cmp new-min min-var)
                (loop (car xs) new-min (cdr xs))
                (loop min min-var (cdr xs))))))))

(define [g-matrix? m] ((listof list?) m))

(define [matrix? m]
  ((and/c g-matrix?
          (or/c null?
                (lambda [m]
                  (letin [firstlen (length (first m))]
                         (andmap (= _ firstlen) (map length (rest m)))))))
   m))

(define [square-matrix? m]
  ((and/c matrix?
          (or/c null?
                (lambda [m] (= (length m) (length (first m))))))
   m))

(define [matrix-ref m x y]
  (list-ref (list-ref m y) x))

(define [matrix-show m [pad-string " "] [align 'right]]
  (letin
   [lenm (length m)]
   [strs (matrix-map ~a m)]
   [flattened (map string-length (flatten strs))]
   [maxlen (if (null? flattened)
               0
               (argmax identity flattened))]
   [padded (matrix-map
            (~a _
                #:pad-string pad-string
                #:min-width maxlen
                #:align align)
            strs)]
   (string-join (map (string-join _
                                  " | "
                                  #:before-first "| "
                                  #:after-last " |")
                     padded)
                "\n")))

(define [matrix-print m] (printf "~a\n" (matrix-show m)))

(define [matrix-map f m]
  (map (map (f _) _) m))

(define [matrix-map-i f m]
  (map (lambda [row y]
         (map (lambda [e x]
                (f e x y))
              row
              (range (length row))))
       m
       (range (length m))))

(define [matrix-empty? m] (or (null? m) (null? (first m))))
(define [matrix-width m] (length (car m)))
(define [matrix-height m] (length m))

(define [matrix-focus m x y w h #:out-of-range-bad? (check? #f)]
  (((map (list-focus x w _ #:out-of-range-bad? check?) _)
    .. (list-focus y h _ #:out-of-range-bad? check?))
   m))

(define [matrix-focus-frames m w h #:out-of-range-bad? (check? #f)]
  (map (lm-list y x
                (matrix-focus m
                              x y
                              w h
                              #:out-of-range-bad? check?))
       (cartesian-product (range 0 (ceiling-multiple-of h (length m)) h)
                          (range 0 (ceiling-multiple-of w (length (car m))) w))))

(define [matrix-append-right m1 m2]
  (if (matrix-empty? m1)
      m2
      (letin
       [lm1 (length m1)]
       [lm2 (length m2)]
       [do (when (> lm2 lm1)
             (error "Cannot append matrixes because height of right matrix is bigger than height of left one"))]
       (map (lm y
                (if (< y lm2)
                    (append (list-ref m1 y) (list-ref m2 y))
                    (list-ref m1 y)))
            (range (length m1))))))

(define [matrix-append-down m1 m2]
  (append m1 m2))

(define [matrix-rebuild-from-frames frames w]
  (apploop [ret buf left] [null (list null) frames]
           (if (null? left)
               (matrix-append-down ret buf)
               (letin
                [head (car left)]
                [tail (cdr left)]
                [len (length (first buf))]
                (if (>= len w)
                    (loop (matrix-append-down ret buf)
                          head
                          tail)
                    (loop ret
                          (matrix-append-right buf head)
                          tail))))))

(define [take-max-common . lists]
  (letin
   [m (argmin identity (map length lists))]
   (map (lambda [x] (take x m)) lists)))

(define [take-atmost n l]
  (if (or (= 0 n) (null? l))
      '()
      (cons (car l) (take-atmost (sub1 n) (cdr l)))))

(define [zip . lists]
  (letrec [[minlen (maximize < length lists)]
           [taken (map (take _ minlen) lists)]]
    (apply map (list* list taken))))

(define [nullable? x]
  (or (equal? #f x) (empty? x) (void? x)))

(define [dirnames-of path]
  (define [loop path]
    (let-values [[[base filename root?] (split-path path)]]
      (if base
          (list* base (loop (path->string base)))
          '())))
  (reverse (loop path)))

(define [make-directories path]
  (define [create-maybe dir]
    (unless (directory-exists? dir)
      (make-directory dir)))
  (let [[directories (dirnames-of path)]]
    (map create-maybe directories)))

;; like do syntax in haskell
(define-syntax letin-with-full
  (syntax-rules ()
    [(letin-with-full f body) body]
    [(letin-with-full f (a b) body ...)
     (f (quote a)
        (quote b)
        b
        (lambda [a]
          (letin-with-full f
                           body
                           ...)))]))

(define letin-global-monad-parameter (make-parameter #f))
(define-syntax-rule [letin-parameterize f . body]
  (parameterize [[letin-global-monad-parameter f]]
    (begin . body)))

;; with parameterization
(define-syntax-rule [letin-with-full-parameterized f . argv]
  (let [[p (letin-global-monad-parameter)]]
    (if p
        (letin-with-full (p f (quote f)) . argv)
        (letin-with-full f . argv))))

(define-syntax-rule [letin-with f . argv]
  (letin-with-full-parameterized (lambda [name result x cont] (f x cont)) . argv))

(define-syntax-rule [dom . argv] (letin-with . argv))

;; Short circuits with any predicate
(define [dom-default default?]
  (lm x cont
      (if (default? x)
          x
          (cont x))))

;; Logs computations
(define [dom-print name result x cont]
  (printf "(~a = ~a = ~a)\n" name x result)
  (cont x))

(define [ceiling-multiple-of mult n]
  (* mult (ceiling (/ n mult))))

(struct cycle-list { buffor } #:prefab)

(struct skipped-list { base indexes }
  #:guard
  (lambda [base indexes name]
    (unless (= (length base) (length indexes))
      (error "skipped list indexes should be the same length as base"))
    (values base indexes)))

(define [make-skipped-list l f]
  (skipped-list l (map f l)))

(define [list-focus-strict start len L]
  (((take _ len) .. (drop _ start)) L))

(define [list-focus-relaxed start len L]
  (letin [L-len (length L)]
         (if (<= L-len start)
             '()
             (letin [droped (drop L start)]
                    (if (<= L-len (+ start len))
                        droped
                        (take droped len))))))

(define [list-focus start len L #:out-of-range-bad? (out-of-range-bad? #t)]
  (if out-of-range-bad?
      (list-focus-strict start len L)
      (list-focus-relaxed start len L)))

(define [list-ref<mod> L index]
  (define [loop cur i len]
    (if (empty? cur)
        (list-ref L (remainder index len))
        (if (= 0 i)
            (first cur)
            (loop (rest cur) (sub1 i) (add1 len)))))
  (loop L index 0))

(define [skipped-list-map f slist]
  (define [loop left left-indexes]
    (if (empty? left)
        '()
        (let [[curr
               (if (first left-indexes)
                   (f (first left))
                   (first left))]]
          (cons curr (loop (rest left) (rest left-indexes))))))
  (let [[new-base (loop (skipped-list-base slist)
                        (skipped-list-indexes slist))]]
    (skipped-list new-base (skipped-list-indexes slist))))

(define [skipped-list-ref index slist]
  (define [loop i left left-indexes]
    (if (empty? left)
        (error "Index out of range")
        (if (first left-indexes)
            (if (= i index)
                (first left)
                (loop (add1 i) (rest left) (rest left-indexes)))
            (loop i (rest left) (rest left-indexes)))))
  (loop 0
        (skipped-list-base slist)
        (skipped-list-indexes slist)))

(gfunc/define g/empty?)

(gfunc/instance g/empty?
 [list?]
 (null? _))

(gfunc/instance g/empty?
 [skipped-list?]
 (lm slist (andmap identity (skipped-list-indexes slist))))

(gfunc/instance g/empty?
 [cycle-list?]
 (lm clist (null? (cycle-list-buffor clist))))

(gfunc/define g/map)

(gfunc/instance g/map
 [procedure? list?]
 (lm f lst (map f lst)))

(gfunc/instance g/map
 [procedure? cycle-list?]
 (lm f clist (map f clist)))

(gfunc/instance g/map
 [procedure? skipped-list?]
 (lm f slist (skipped-list-map f slist)))

(gfunc/define g/ref)

(gfunc/instance g/ref
 [integer? list?]
 (lm i lst (list-ref lst i)))

(gfunc/instance g/ref
 [integer? cycle-list?]
 (lm i clist (list-ref<mod> (cycle-list-buffor clist) i)))

(gfunc/instance g/ref
 [integer? skipped-list?]
 (lm i slist (skipped-list-ref i slist)))

