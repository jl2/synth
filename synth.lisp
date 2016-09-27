;;;; synth.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:synth)
(named-readtables:in-readtable :qtools)

(declaim (optimize (speed 3) (safety 0) (size 0) (debug 0)))

(defparameter *fps* 30)
(defparameter *fft-window-size* 1024)

(defclass synth-streamer ()
  ((freq  :initform 440.0d0)
   (phase :initform 0.0d0)))

(defmethod mixalot:streamer-mix-into ((streamer synth-streamer) mixer buffer offset length time)
  (declare (ignore time))
  (with-slots (n freq phase playing) streamer
    (loop for index upfrom offset
       repeat length
       with dp = (* 2.0 pi freq 1/44100)
       as sample = (round (* 7000 (sin phase)))
       do
         (mixalot:stereo-mixf (aref buffer index) (mixalot:mono->stereo sample))
         (incf phase dp))))

(defun make-streamer-for-frequency (the-freq)
  (let ((stream (make-instance 'synth-streamer)))
    (with-slots (freq) stream
      (setf freq the-freq))
    stream))
      


(define-widget main-window (QMainWindow)
  ((mixer    :initform (mixalot:create-mixer))
   (note-map :initform (list 
                        (cons (q+:qt.key_a) (make-streamer-for-frequency 440.0))
                        (cons (q+:qt.key_s) (make-streamer-for-frequency 466.16))
                        (cons (q+:qt.key_d) (make-streamer-for-frequency 493.88))
                        (cons (q+:qt.key_f) (make-streamer-for-frequency 523.25))
                        (cons (q+:qt.key_v) (make-streamer-for-frequency 554.36))
                        (cons (q+:qt.key_t) (make-streamer-for-frequency 587.33))
                        (cons (q+:qt.key_y) (make-streamer-for-frequency 622.25))
                        (cons (q+:qt.key_n) (make-streamer-for-frequency 659.26))
                        (cons (q+:qt.key_j) (make-streamer-for-frequency 698.46))
                        (cons (q+:qt.key_k) (make-streamer-for-frequency 739.99))
                        (cons (q+:qt.key_l) (make-streamer-for-frequency 783.99))
                        (cons (q+:qt.key_semicolon) (make-streamer-for-frequency 830.61))))))

(define-widget synth-gl-widget (QGLWidget)
  ((window-buffer :initform (make-array *fft-window-size* 
                                        :element-type '(complex double-float)
                                        :adjustable nil
                                        :fill-pointer nil))
   (left-fft-data :initform (make-array *fft-window-size* 
                                        :element-type '(complex double-float)
                                        :adjustable nil
                                        :fill-pointer nil))
   (right-fft-data :initform (make-array *fft-window-size* 
                                         :element-type '(complex double-float)
                                         :adjustable nil
                                         :fill-pointer nil)))
  (:documentation "The main synthesizer view widget."))

(define-subwidget (synth-gl-widget timer) (q+:make-qtimer synth-gl-widget)
  (setf (q+:single-shot timer) nil))

(define-initializer (synth-gl-widget setup)
  (q+:start timer (round (/ 1000 *fps*)))
  (setf (q+:auto-fill-background synth-gl-widget) nil)
  (setf (q+:auto-buffer-swap synth-gl-widget) nil))

(define-slot (synth-gl-widget tick) ()
  (declare (connected timer (timeout)))
  (q+:repaint synth-gl-widget))

(define-override (synth-gl-widget initialize-G-L) ()
  (gl:enable :line-smooth :polygon-smooth
             :depth-test :depth-clamp :alpha-test))

(define-override (synth-gl-widget resize-g-l) (width height)
  )

(define-override (synth-gl-widget paint-g-l paint) ()
  "Handle paint events."

  (let* ((width (q+:width synth-gl-widget))
         (height (q+:height synth-gl-widget))
         (x-aspect-ratio (if (< height width)
                             (/ height width 1.0d0)
                             1.0d0))
         (y-aspect-ratio (if (< height width)
                             1.0d0
                             (/ width height 1.0d0))))

    (with-finalizing ((painter (q+:make-qpainter synth-gl-widget)))

      (q+:begin-native-painting painter)
      (gl:viewport 0 0 width height)

      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho -1.0 1.0 -1.0 1.0 -1.0 1.0)

      (gl:clear-color 0 0 0 1)
      (gl:enable :line-smooth :polygon-smooth
                 :depth-test :depth-clamp :alpha-test)

      (gl:matrix-mode :modelview)
      (gl:load-identity)

      (gl:clear :color-buffer :depth-buffer)

      (gl:push-matrix)

      (gl:with-primitives :lines
        (gl:color 1 0 0)
        (gl:vertex -1.0 1.0 0.0)
        (gl:vertex 1.0 -1.0 0.0)

        (gl:vertex -1.0 -1.0 0.0)
        (gl:vertex 1.0 1.0 0.0))
      (gl:pop-matrix)

      (q+:swap-buffers synth-gl-widget)
      (q+:end-native-painting painter))))


(define-override (main-window close-event) (ev)
  (mixalot:mixer-remove-all-streamers mixer)
  (mixalot:destroy-mixer mixer)
  (q+:accept ev))

(define-override (main-window key-press-event) (ev)
  (cond
    ;; Catch escape to forbid removing text before input.
    ((and
      (not (q+:is-auto-repeat ev))
      (find (q+:key ev) note-map :key #'car))
     (mixalot:mixer-add-streamer mixer (cdr (assoc (q+:key ev) note-map)))
     (call-next-qmethod))

    ;; Delegate standard.
    (T
     (call-next-qmethod))))

(define-override (main-window key-release-event) (ev)
  (cond

    ((and
      (not (q+:is-auto-repeat ev))
      (find (q+:key ev) note-map :key #'car))
     (mixalot:mixer-remove-streamer mixer (cdr (assoc (q+:key ev) note-map)))
     (call-next-qmethod))

    ;; Delegate standard.
    (T
     (call-next-qmethod))))

(define-menu (main-window File)
  (:separator)
  (:item ("Quit" (ctrl alt q))
         (q+:close main-window)))

(define-menu (main-window Help)
  (:item "About"
         (q+:qmessagebox-information
          main-window "About"
          "Rudimentary Synthesizer.")))

(define-subwidget (main-window synth-viewer) (make-instance 'synth-gl-widget)
  "The central synthesizer viewer")

(define-initializer (main-window setup)
  "Set the window title and set the synth-viewer to be the central widget."
  (setf (q+:window-title main-window) "Synthesizer")
  (setf (q+:central-widget main-window) synth-viewer))

(defun main ()
  "Create the main window."
  (trivial-main-thread:call-in-main-thread #'mixalot:main-thread-init)
  (with-main-window (window (make-instance 'main-window))))
