;;;; synth.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:synth
  :description "Describe synth here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:trivial-main-thread
               #:bordeaux-fft
               #:mixalot
               #:qt
               #:qtgui
               #:qtcore
               #:qtools
               #:qtopengl
               #:cl-opengl
               #:cl-glu)
  :serial t
  :components ((:file "package")
               (:file "synth")))

