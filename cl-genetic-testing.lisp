;;;; cl-genetic-testing.lisp

(in-package #:cl-genetic-testing)

;;; "cl-genetic-testing" goes here. Hacks and glory await!

(defgeneric run (program &rest args)
  (:documentation "Runs the given program with the given args"))
(defgeneric generate-random (program-class &key &allow-other-keys)
  (:documentation "Generates a random member of the given class of programs."))
(defgeneric make-child (program1 program2)
  (:documentation "Generates a child of the two programs"))
(defgeneric mutate (program)
  (:documentation "Introduces a random mutation into the program"))
(defclass program () ()
  (:documentation "Root class of all genetic programs."))


(defparameter *population-size* 50)
(defparameter *generation-size* 10)
(defparameter *mutation-rate* 0.001)

(defun range (n)
  (iter:iter (iter:for i :below n)
    (iter:collect i)))

(defun solve (program-class test-sets max-generations &rest other-args
              &key
                initial-population
                (population-size (if initial-population
                                     (length initial-population)
                                     *population-size*))
                (generation-size *generation-size*)
                (mutation-rate *mutation-rate*)
                (min-error 0)
              &allow-other-keys)
  (labels ((select-best-program-lexicase (errors prog-idxs)
             "Uses lexicase selection to choose a 'best' program for the next generation"
             (iter:iter
               (iter:with error-idxs := (alexandria:shuffle (range (length (elt errors 0)))))
               (iter:initially (setf remaining-prog-idxs prog-idxs))
               (iter:while (cdr remaining-prog-idxs))
               (iter:for error-idx :in error-idxs)
               (iter:for min-error-val :next
                         (iter:iter
                           (iter:for prog-idx :in remaining-prog-idxs)
                           (iter:for errs :next (elt errors prog-idx))
                           (iter:minimize (elt errs error-idx))))
               (iter:for remaining-prog-idxs :next (delete-if-not (lambda (e) (= e min-error-val))
                                                                  remaining-prog-idxs
                                                                  :key (lambda (p) (elt (elt errors p)
                                                                                   error-idx))))
               (iter:finally (return remaining-prog-idxs))))
           (select-worst-program (errors prog-idxs)
             (let* ((max-error-val (iter:iter (iter:for idx :below (length errors))
                                     (iter:maximize (reduce #'+ (elt errors idx))))))
               (delete-if-not (lambda (e)
                                (= max-error-val
                                   (reduce #'+ (elt errors e))))
                              prog-idxs)))
           (select-worst-program-lexicase (errors prog-idxs)
             "Uses lexicase selection to choose a 'worst' program for the next generation"
             (iter:iter
               (iter:with error-idxs := (alexandria:shuffle (range (length (elt errors 0)))))
               (iter:initially (setf remaining-prog-idxs prog-idxs))
               (iter:while (cdr remaining-prog-idxs))
               (iter:for error-idx :in error-idxs)
               (iter:for max-error-val :next
                         (iter:iter
                           (iter:for prog-idx :in remaining-prog-idxs)
                           (iter:for errs :next (elt errors prog-idx))
                           (iter:maximize (elt errs error-idx))))
               (iter:for remaining-prog-idxs :next (delete-if-not (lambda (e) (= e max-error-val))
                                                                  remaining-prog-idxs
                                                                  :key (lambda (p) (elt (elt errors p)
                                                                                   error-idx))))
               (iter:finally (return remaining-prog-idxs)))))
    (let* ((population (coerce
                        (or initial-population
                            (iter:iter (iter:repeat population-size)
                              (iter:collect (apply #'generate-random program-class other-args))))
                        'vector))
           (test-sets (reverse test-sets))
           (errors (iter:iter (iter:for tests :in test-sets)
                     (iter:collect
                         (coerce (iter:iter (iter:for i :below population-size)
                                   (iter:collect (mapcar (lambda (test)
                                                           (funcall test (elt population i)))
                                                         tests)))
                                 'vector)))))
      (iter:iter outer
        (iter:repeat max-generations)
        (iter:generate parent1-idx :next (alexandria:random-elt
                                          (reduce #'select-best-program-lexicase
                                                  errors
                                                  :initial-value (range population-size)
                                                  :from-end t)))
        (iter:iter
          (iter:repeat generation-size)
          (iter:for parent1 :next (elt population (iter:in outer (iter:next parent1-idx))))
          (iter:for parent2 :next (elt population
                                       (alexandria:random-elt
                                        (reduce #'select-best-program-lexicase
                                                errors
                                                :initial-value (range population-size)
                                                :from-end t))))
          (iter:for worst-idx :next (alexandria:random-elt
                                     (reduce #'select-worst-program-lexicase
                                             errors
                                             :initial-value (range population-size)
                                             :from-end t)))
          (setf (elt population worst-idx) (make-child parent1 parent2))
          (if (< (random 1.0) mutation-rate)
              (mutate (elt population worst-idx)))
          (iter:iter (iter:for i :below (length errors))
            (setf (elt (elt errors i) worst-idx) (mapcar
                                                  (lambda (test)
                                                    (funcall test (elt population worst-idx)))
                                                  (elt test-sets i))))
          (iter:finally (let ((errors (iter:iter (iter:for err :in errors)
                                        (iter:appending (elt err parent1-idx)))))
                          (print errors)
                          (if (<= (reduce #'+ errors) min-error)
                              (return-from solve (values population
                                                         parent1))))))
        (iter:finally (return-from solve (values population (elt population parent1-idx))))))))

(defun dataset->funcs (dataset)
  (iter:iter (iter:for point :in dataset)
    (iter:collect (let ((p point))
                    (lambda (prog)
                      (let ((result (apply #'run prog (rest p))))
                        (typecase (first p)
                          (number (abs (- (first p) result)))
                          (t (if (equal (first p) result)
                                 0
                                 1)))))))))
