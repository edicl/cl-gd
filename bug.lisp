;; -*- Lisp -*-

(defpackage :bug
  (:use :cl))

(in-package :bug)

(ql:quickload "cl-gd")
(use-package :cl-gd)

(let* ((x1 -86)
       (x2 529)
       (y1 -0.13)
       (y2 1.16)
       (xv '(0.0 1.0 2.0 3.0 4.0))
       (yv '(0.99 0.12 0.66 0.24 0.075)))
  (with-image* (500 250)
    (allocate-color 255 255 255)
    (let ((c (allocate-color 0 0 255)))
      (with-transformation (:x1 x1 :x2 x2 :y1 y1 :y2 y2)
        (with-thickness (1)
          (mapl #'(lambda (x y)
                    (when (and (cdr x) (cdr y))
                      (draw-line (car x) (car y) (cadr x) (cadr y) :color c))) xv yv))
        (write-image-to-file "test.png" :if-exists :supersede)))))

