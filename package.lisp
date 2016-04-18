;;;; package.lisp

(defpackage #:cl-genetic-testing
  (:use #:cl)
  (:export #:run
           #:generate-random
           #:make-child
           #:mutate
           #:program
           #:range
           #:solve
           #:dataset->funcs))

(defpackage #:cl-genetic-testing.cl
  (:use #:cl
        #:cl-genetic-testing))
