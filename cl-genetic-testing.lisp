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

(defun solve (program-class tests max-generations &rest other-args
              &key
                initial-population
                (population-size (if initial-population
                                     (length initial-population)
                                     *population-size*))
                (generation-size *generation-size*)
                (mutation-rate *mutation-rate*)
                (min-error 0)
              &allow-other-keys)
  (labels ((select-best-program-lexicase (errors)
             "Uses lexicase selection to choose a 'best' program for the next generation"
             (iter:iter
               (iter:with error-idxs := (alexandria:shuffle (range (length (elt errors 0)))))
               (iter:initially (setf remaining-prog-idxs (range (length errors))))
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
               (iter:finally (return (alexandria:random-elt remaining-prog-idxs)))))
           (select-worst-program (errors)
             (let* ((max-error-val (iter:iter (iter:for idx :below (length errors))
                                     (iter:maximize (reduce #'+ (elt errors idx))))))
               (iter:iter (iter:for idx :below (length errors))
                 (if (= max-error-val
                        (reduce #'+ (elt errors idx)))
                     (return idx)))))
           (select-worst-program-lexicase (errors)
             "Uses lexicase selection to choose a 'worst' program for the next generation"
             (iter:iter
               (iter:with error-idxs := (alexandria:shuffle (range (length (elt errors 0)))))
               (iter:initially (setf remaining-prog-idxs (range (length errors))))
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
               (iter:finally (return (alexandria:random-elt remaining-prog-idxs))))))
    (let* ((population (coerce
                        (or initial-population
                            (iter:iter (iter:repeat population-size)
                              (iter:collect (apply #'generate-random program-class other-args))))
                        'vector))
           (errors (coerce (iter:iter (iter:for i :below population-size)
                             (iter:collect (mapcar (lambda (test)
                                                     (funcall test (elt population i)))
                                                   tests)))
                           'vector)))
      (iter:iter (iter:repeat max-generations)
        (iter:iter
          (iter:repeat generation-size)
          (iter:for parent1-idx :next (select-best-program-lexicase errors))
          (iter:for parent1 :next (elt population parent1-idx))
          (iter:for parent2 :next (elt population (select-best-program-lexicase errors)))
          (iter:for worst-idx :next (select-worst-program-lexicase errors))
          (setf (elt population worst-idx) (make-child parent1 parent2))
          (if (< (random 1.0) mutation-rate)
              (mutate (elt population worst-idx)))
          (setf (elt errors worst-idx) (mapcar (lambda (test)
                                                 (funcall test (elt population worst-idx)))
                                               tests))
          (iter:finally (let ((errors (elt errors parent1-idx)))
                          (print errors)
                          (if (<= (reduce #'+ errors) min-error)
                              (return-from solve (values population
                                                         parent1)))))))
      (values population nil))))

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
