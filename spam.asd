(defpackage :spam-asd
  (:use :cl :asdf))

(in-package :spam-asd)

(defsystem :spam
  :name "spam"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Naive Bayesian Filter for LispWorks."
  :serial t
  :components ((:file "spam"))
  :depends-on ("re"))
