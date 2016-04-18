;;;; cl-genetic-testing.asd

(asdf:defsystem #:cl-genetic-testing
  :description "Describe cl-genetic-testing here"
  :author "Elijah Malaby"
  :license "Specify license here"
  :depends-on (#:iterate #:alexandria)
  :serial t
  :components ((:file "package")
               (:file "cl-genetic-testing")
               (:file "cl")))
