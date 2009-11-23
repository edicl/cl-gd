;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/cl-gd-test.lisp,v 1.27 2009/11/23 17:05:38 edi Exp $

;;; Copyright (c) 2003-2009, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage #:cl-gd-test
  (:use #:cl
        #:cl-gd)
  (:export #:test))

(in-package :cl-gd-test)

(defparameter *test-directory*
  (merge-pathnames (make-pathname :directory '(:relative "test"))
                   (make-pathname :name nil
                                  :type nil
                                  :version :newest
                                  :defaults cl-gd.system:*cl-gd-directory*))
  
  "Where test files are put.")

(defun test-file-location (name &optional (type :unspecific))
  "Create test file location from NAME and TYPE component."
  (make-pathname :name name
                 :type type
                 :defaults *test-directory*))

(defun compare-files (file &key type expected-result)
  "Compare test file FILE to orginal file in subdirectory ORIG."
  (with-image-from-file (image file)
    (with-image-from-file (orig (merge-pathnames
                                 (make-pathname :type
                                                (or type (pathname-type file))
                                                :directory
                                                '(:relative "orig"))
                                 file))
      (equal (differentp image orig)
             expected-result))))

(defun test-001 ()
  (let ((file (test-file-location "one-pixel" "png")))
    ;; 40x40 image
    (with-image* (40 40)
      ;; white background
      (allocate-color 255 255 255)
      ;; black pixel in the middle
      (set-pixel 20 20 :color (allocate-color 0 0 0))
      ;; write to PNG target
      (write-image-to-file file :if-exists :supersede))
    ;; compare to existing file
    (compare-files file)))

(defun test-002 ()
  (let ((file (test-file-location "one-pixel" "jpg")))
    ;; 40x40 image
    (with-image* (40 40)
      ;; white background
      (allocate-color 255 255 255)
      ;; black pixel in the middle
      (set-pixel 20 20 :color (allocate-color 0 0 0))
      ;; write to JPEG target
      (write-image-to-file file :if-exists :supersede))
    ;; compare to existing file
    (compare-files file)))

(defun test-003 ()
  (let ((file (test-file-location "one-line" "png")))
    ;; 40x40 image
    (with-image* (40 40)
      ;; white background
      (allocate-color 255 255 255)
      ;; anti-aliased black line
      (draw-line 20 20 30 30
                 :color (make-anti-aliased
                         (allocate-color 0 0 0)))
      ;; write to PNG target
      (write-image-to-file file :if-exists :supersede))
    ;; compare to existing file
    (compare-files file)))

(defun test-004 ()
  (let ((file (test-file-location "one-line" "jpg")))
    ;; 40x40 image
    (with-image* (40 40)
      ;; white background
      (allocate-color 255 255 255)
      ;; anti-aliased black line
      (draw-line 20 20 30 30
                 :color (make-anti-aliased
                         (allocate-color 0 0 0)))
      ;; write to JPEG target
      (write-image-to-file file :if-exists :supersede))
    ;; compare to existing PNG file
    (compare-files file)))

(defun test-005 ()
  (with-image-from-file* ((test-file-location "one-pixel" "png"))
    (let ((num (number-of-colors)))
      (find-color 255 255 255 :resolve t)
      (multiple-value-bind (width height)
          (image-size)
        (and (= width 40)
             (= height 40)
             ;; FIND-COLOR should not have changed the number of
             ;; colors
             (= num (number-of-colors)))))))

(defun test-006 ()
  (with-image-from-file* ((test-file-location "one-pixel" "png"))
    (with-transformation (:x1 0.1 :x2 0.5 :y1 10.8 :y2 20.9)
      (multiple-value-bind (width height)
          (image-size)
        ;; make sure WITH-TRANSFORMATION returns transformed size
        (and (>= 0.0001 (abs (- 0.4 width)))
             (>= 0.0001 (abs (- 10.1 height))))))))

(defun test-007 ()
  (let ((file (test-file-location "circle" "png")))
    (with-image* (40 40)
      (allocate-color 255 255 255)
      (let ((black (allocate-color 0 0 0)))
        (with-default-color (black)
          ;; move origin to center and stretch
          (with-transformation (:x1 -100 :width 200 :y1 -100 :height 200)
            (draw-filled-circle 0 0 50)
            (write-image-to-file file
                                 :if-exists :supersede)))))
    (compare-files file)))

(defun test-008 ()
  (with-image (image 40 40)
    (allocate-color 255 255 255 :image image)
    (with-default-color ((allocate-color 0 0 0 :image image))
      ;; no transformation and use more general ellipse function
      (draw-filled-ellipse 20 20 20 20 :image image)
      (with-image-from-file (other-image
                             (test-file-location "circle" "png"))
        (not (differentp image other-image))))))

(defun test-009 ()
  (let ((file (test-file-location "chart" "png")))
    ;; create 200x200 pixel image 
    (with-image* (200 200)
      ;; background color
      (allocate-color 68 70 85)
      (let ((beige (allocate-color 222 200 81))
            (brown (allocate-color 206 150 75))
            (green (allocate-color 104 156 84))
            (red (allocate-color 163 83 84))
            (white (allocate-color 255 255 255))
            (two-pi (* 2 pi)))
        ;; move origin to center of image
        (with-transformation (:x1 -100 :x2 100 :y1 -100 :y2 100 :radians t)
          ;; draw some 'pie slices'
          (draw-arc 0 0 130 130 0 (* .6 two-pi)
                    :center-connect t :filled t :color beige)
          (draw-arc 0 0 130 130 (* .6 two-pi) (* .8 two-pi)
                    :center-connect t :filled t :color brown)
          (draw-arc 0 0 130 130 (* .8 two-pi) (* .95 two-pi)
                    :center-connect t :filled t :color green)
          (draw-arc 0 0 130 130 (* .95 two-pi) two-pi
                    :center-connect t :filled t :color red)
          ;; use GD fonts
          (with-default-color (white)
            (with-default-font (:small)
              (draw-string -8 -30 "60%")
              (draw-string -20 40 "20%")
              (draw-string 20 30 "15%"))
            (draw-string -90 90 "Global Revenue"
                         :font :large))
          (write-image-to-file file
                               :compression-level 6
                               :if-exists :supersede))))
    (compare-files file)))
  
(defun test-010 ()
  (let ((file (test-file-location "zappa-green" "jpg")))
    ;; get JPEG from disk
    (with-image-from-file (old (test-file-location "zappa" "jpg"))
      (multiple-value-bind (width height)
          (image-size old)
        (with-image (new width height)
          ;; green color for background
          (allocate-color 0 255 0 :image new)
          ;; merge with original JPEG
          (copy-image old new 0 0 0 0 width height
                      :merge 50)
          (write-image-to-file file
                               :image new
                               :if-exists :supersede))))
    (compare-files file)))

(defun test-011 ()
  ;; small image
  (with-image* (10 10)
    (loop for i below +max-colors+ do
          ;; allocate enough colors (all gray) to fill the palette
          (allocate-color i i i))
    (and (= +max-colors+ (number-of-colors))
         (null (find-color 255 0 0 :exact t))
         (let ((match (find-color 255 0 0))) ; green
           (and (= 85
                   (color-component :red match)
                   (color-component :green match)
                   (color-component :blue match)))))))

(defun test-012 ()
  (let ((file (test-file-location "triangle" "png")))
    (with-image* (100 100)
      (allocate-color 255 255 255) ; white background
      (let ((red (allocate-color 255 0 0))
            (yellow (allocate-color 255 255 0))
            (orange (allocate-color 255 165 0)))
        ;; thin black border
        (draw-rectangle* 0 0 99 99
                         :color (allocate-color 0 0 0))
        ;; lines are five pixels thick
        (with-thickness (5)
          ;; colored triangle
          (draw-polygon (list 10 10 90 50 50 90)
                        ;; styled color
                        :color (list red red red
                                     yellow yellow yellow
                                     nil nil nil
                                     orange orange orange))
          (write-image-to-file file
                               :compression-level 8
                               :if-exists :supersede))))
    (compare-files file)))

(defun test-013 ()
  (let ((file (test-file-location "brushed-arc" "png")))
    (with-image* (200 100)
      (allocate-color 255 165 0) ; orange background
      (with-image (brush 6 6)
        (let* ((black (allocate-color 0 0 0 :image brush)) ; black background
               (red (allocate-color 255 0 0 :image brush))
               (blue (allocate-color 0 0 255 :image brush)))
          (setf (transparent-color brush) black) ; make background transparent
          ;; now set the pixels in the brush
          (set-pixels '(2 2 2 3 3 2 3 3)
                      :color blue :image brush)
          (set-pixels '(1 2 1 3 4 2 4 3 2 1 3 1 2 4 3 4)
                      :color red :image brush)
          ;; then use it to draw an arc
          (draw-arc 100 50 180 80 180 300 :color (make-brush brush)))
        (write-image-to-file file
                             :compression-level 7
                             :if-exists :supersede)))
    (compare-files file)))

(defun test-014 ()
  (let ((file (test-file-location "anti-aliased-lines" "png")))
    (with-image* (150 50)
      (let ((orange (allocate-color 255 165 0)) ; orange background
            (white (allocate-color 255 255 255))
            (red (allocate-color 255 0 0)))
        ;; white background rectangle in the middle third
        (draw-rectangle* 50 0 99 49
                         :filled t
                         :color white)
        (with-thickness (2)
          ;; just a red line
          (draw-line 5 10 145 10 :color red)
          ;; anti-aliased red line
          (draw-line 5 25 145 25 :color (make-anti-aliased red))
          ;; anti-aliased red line which should stand out against
          ;; orange background
          (draw-line 5 40 145 40 :color (make-anti-aliased red orange))))
      (write-image-to-file file
                           :compression-level 3
                           :if-exists :supersede))
    (compare-files file)))

(defun test-015 ()
  (let ((file (test-file-location "clipped-tangent" "png")))
    (with-image* (150 150)
      (allocate-color 255 255 255) ; white background
      ;; transform such that x axis ranges from (- PI) to PI and y
      ;; axis ranges from -3 to 3
      (with-transformation (:x1 (- pi) :width (* 2 pi) :y1 -3 :y2 3)
        (let ((black (allocate-color 0 0 0))
              (red (allocate-color 255 0 0))
              (rectangle (list (- .4 pi) 2.5 (- pi .4) -2.5)))
          (with-default-color (black)
            ;; draw axes
            (draw-line 0 -3 0 3 :color black)
            (draw-line (- pi) 0 pi 0))
          ;; show clipping rectangle (styled)
          (draw-rectangle rectangle :color (list black black black nil black nil))
          (with-clipping-rectangle (rectangle)
            ;; draw tangent function
            (loop for x from (- pi) below (* 2 pi) by (/ pi 75) do
                  (set-pixel x (tan x) :color red)))))
      (write-image-to-file file
                           :if-exists :supersede))
    (compare-files file)))

(defun gd-demo-picture (file random-state &optional write-file)
  (with-image* ((+ 256 384) 384 t)
    (let ((white (allocate-color 255 255 255))
          (red (allocate-color 255 0 0))
          (green (allocate-color 0 255 0))
          (blue (allocate-color 0 0 255))
          (vertices (list 64 0 0 128 128 128))
          (image-width (image-width))
          (image-height (image-height)))
      (setf (transparent-color) white)
      (draw-rectangle* 0 0 image-width image-height :color white)
      (with-image-from-file (in-file (test-file-location "demoin" "png"))
        (copy-image in-file *default-image*
                    0 0 32 32 192 192
                    :resize t
                    :dest-width 255
                    :dest-height 255
                    :resample t)
        (multiple-value-bind (in-width in-height)
            (image-size in-file)
          (loop for a below 360 by 45 do
                (copy-image in-file *default-image*
                            0 0
                            (+ 256 192 (* 128 (cos (* a .0174532925))))
                            (- 192 (* 128 (sin (* a .0174532925))))
                            in-width in-height
                            :rotate t
                            :angle a))
          (with-default-color (green)
            (with-thickness (4)
              (draw-line 16 16 240 16)
              (draw-line 240 16 240 240)
              (draw-line 240 240 16 240)
              (draw-line 16 240 16 16))
            (draw-polygon vertices :filled t))
          (dotimes (i 3)
            (incf (nth (* 2 i) vertices) 128))
          (draw-polygon vertices
                        :color (make-anti-aliased green)
                        :filled t)
          (with-default-color (blue)
            (draw-arc 128 128 60 20 0 720)
            (draw-arc 128 128 40 40 90 270)
            (fill-image 8 8))
          (with-image (brush 16 16 t)
            (copy-image in-file brush
                        0 0 0 0
                        in-width in-height
                        :resize t
                        :dest-width (image-width brush)
                        :dest-height (image-height brush))
            (draw-line 0 255 255 0
                       :color (cons (make-brush brush)
                                    (list nil nil nil nil nil nil nil t))))))
      (with-default-color (red)
        (draw-string 32 32 "hi" :font :giant)
        (draw-string 64 64 "hi" :font :small))
      (with-clipping-rectangle* (0 (- image-height 100) 100 image-height)
        (with-default-color ((make-anti-aliased white))
          (dotimes (i 100)
            (draw-line (random image-width random-state)
                       (random image-height random-state)
                       (random image-width random-state)
                       (random image-height random-state))))))
    (setf (interlacedp) t)
    (true-color-to-palette)
    (if write-file
      (write-image-to-file file
                           :if-exists :supersede)
      (with-image-from-file (demo-file file)
        (not (differentp demo-file *default-image*))))))

(defun test-016 ()
  (let* ((file (test-file-location "demooutp" "png"))
         (random-state-1 (make-random-state t))
         (random-state-2 (make-random-state random-state-1)))
    (gd-demo-picture file random-state-1 t)
    (gd-demo-picture file random-state-2)))

(defun test-017 ()
  (let ((file (test-file-location "zappa-ellipse" "png")))
    (with-image* (250 150)
      (with-image-from-file (zappa (test-file-location "smallzappa" "png"))
        (setf (transparent-color) (allocate-color 255 255 255))
        (draw-filled-ellipse 125 75 250 150
                             :color (make-tile zappa)))
      (write-image-to-file file
                           :if-exists :supersede))
    (compare-files file)))

(defun test-018 ()
  (let (result)
    (with-image* (3 3)
      (allocate-color 255 255 255)
      (draw-line 0 0 2 2 :color (allocate-color 0 0 0))
      (do-rows (y)
        (let (row)
          (do-pixels-in-row (x)
            (push (list x y (raw-pixel)) row))
          (push (nreverse row) result))))
    (equal
     (nreverse result)
     '(((0 0 1) (1 0 0) (2 0 0))
       ((0 1 0) (1 1 1) (2 1 0))
       ((0 2 0) (1 2 0) (2 2 1))))))
    
(defun test-019 ()
  (let (result)
    (with-image* (3 3 t)
      (draw-rectangle* 0 0 2 2 :color (allocate-color 0 0 0))
      (draw-line 0 0 2 2 :color (allocate-color 255 255 255))
      (do-pixels ()
        (unless (zerop (raw-pixel))
          (decf (raw-pixel) #xff)))
      (do-rows (y)
        (let (row)
          (do-pixels-in-row (x)
            (push (list x y (raw-pixel)) row))
          (push (nreverse row) result))))
    (equal
     (nreverse result)
     '(((0 0 #xffff00) (1 0 0) (2 0 0))
       ((0 1 0) (1 1 #xffff00) (2 1 0))
       ((0 2 0) (1 2 0) (2 2 #xffff00))))))

(defun test-020 (georgia)
  ;; not used for test suite because of dependency on font
  (with-image* (200 200)
    ;; set background (white) and make it transparent
    (setf (transparent-color)
            (allocate-color 255 255 255))
    (loop for angle from 0 to (* 2 pi) by (/ pi 6)
          for blue downfrom 255 by 20 do
          (draw-freetype-string 100 100 "Common Lisp"
                                :font-name georgia
                                :angle angle
                                ;; note that ALLOCATE-COLOR won't work
                                ;; here because the anti-aliasing uses
                                ;; up too much colors
                                :color (find-color 0 0 blue
                                                   :resolve t)))
    (write-image-to-file (test-file-location "strings" "png")
                         :if-exists :supersede)))

(defun test% (georgia)
  (loop for i from 1 to (if georgia 20 19) do
        (handler-case
          (format t "Test ~A ~:[failed~;succeeded~].~%" i
                  (let ((test-function
                          (intern (format nil "TEST-~3,'0d" i)
                                  :cl-gd-test)))
                    (if (= i 20)
                      (funcall test-function georgia)
                      (funcall test-function))))
          (error (condition)
            (format t "Test ~A failed with the following error: ~A~%"
                    i condition)))
        (force-output))
  (format t "Done.~%"))

(defun test (&optional georgia)
  #-:sbcl
  (test% georgia)
  #+:sbcl
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    (test% georgia)))