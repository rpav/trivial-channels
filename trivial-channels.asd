(defpackage :trivial-channels.asdf
  (:use #:cl #:asdf))

(in-package :trivial-channels.asdf)

(defsystem :trivial-channels
  :description "Really simple channels and queue"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "1.0"

  :depends-on (:bordeaux-threads :trivial-timeout)
  :serial t

  :components
  ((:file "package")
   (:file "trivial-channels")))
