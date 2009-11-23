;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/drawing.lisp,v 1.29 2009/11/23 17:05:38 edi Exp $

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

(in-package :cl-gd)

(defun get-pixel (x y &key (image *default-image*))
  "Gets the color associated with point \(X,Y)."
  (check-type image image)
  (with-transformed-alternative
      ((x x-transformer)
       (y y-transformer))
    (gd-image-get-pixel (img image) x y)))

(defun set-pixel (x y &key (color *default-color*) (image *default-image*))
  "Draws a pixel with color COLOR at point \(X,Y)."
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((x x-transformer)
         (y y-transformer))
      (gd-image-set-pixel (img image) x y color)))
  (values x y))

(defgeneric set-pixels (points &key color image)
  (:documentation "Draws a list \(X1 Y1 X2 Y2 ...) or vector #\(X1 Y1
X2 Y2 ...) of pixels."))

(defmethod set-pixels ((points list) &key (color *default-color*) (image *default-image*))
  (check-type image image)
  (unless (evenp (length points))
    (error "List ~S must have an even number of elements"
           points))
  (loop with img = (img image)
        for (x y) on points by #'cddr do
        (check-type x integer)
        (check-type y integer)
        (with-transformed-alternative
            ((x x-transformer)
             (y y-transformer))
          (gd-image-set-pixel img x y color))
        finally (return image)))

(defmethod set-pixels ((points vector) &key (color *default-color*) (image *default-image*))
  (check-type image image)
  (let ((length (length points)))
    (unless (evenp length)
      (error "List ~S must have an even number of elements"
             points))
    (loop with img = (img image)
          for i below length by 2 do
          (check-type (aref points i) integer)
          (check-type (aref points (1+ i)) integer)
          (with-transformed-alternative
              (((aref points i) x-transformer)
               ((aref points (1+ i)) y-transformer))
            (gd-image-set-pixel img
                                (aref points i)
                                (aref points (1+ i))
                                color))
          finally (return image))))

(defun draw-line (x1 y1 x2 y2 &key (color *default-color*) (image *default-image*))
  "Draws a line with color COLOR from point \(X1,Y1) to point \(X2,Y2)."
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((x1 x-transformer)
         (y1 y-transformer)
         (x2 x-transformer)
         (y2 y-transformer))
      (gd-image-line (img image) x1 y1 x2 y2 color)))
  (values x1 y1 x2 y2))

(defun draw-rectangle* (x1 y1 x2 y2 &key filled (color *default-color*) (image *default-image*))
  "Draws a rectangle with upper left corner \(X1,Y1) and lower right
corner \(X2,Y2). If FILLED is true the rectangle will be filled with
COLOR, otherwise it will be outlined."
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((x1 x-transformer)
         (y1 y-transformer)
         (x2 x-transformer)
         (y2 y-transformer))
      (if filled
        (gd-image-filled-rectangle (img image) x1 y1 x2 y2 color)
        (gd-image-rectangle (img image) x1 y1 x2 y2 color))))
  (values x1 y1 x2 y2))

(defun draw-rectangle (rectangle &key filled (color *default-color*) (image *default-image*))
  "Draws a rectangle with upper left corner \(X1,Y1) and lower right
corner \(X2,Y2) where RECTANGLE is the list \(X1 Y1 X2 Y2). If FILLED
is true the rectangle will be filled with COLOR, otherwise it will be
outlined."
  (draw-rectangle* (first rectangle)
                   (second rectangle)
                   (third rectangle)
                   (fourth rectangle)
                   :filled filled
                   :color color
                   :image image)
  rectangle)

(defgeneric draw-polygon (vertices &key filled start end color image)
  (:documentation "Draws a polygon with the VERTICES \(at least three)
specified as a list \(x1 y1 x2 y2 ...) or as a vector #\(x1 y1 x2 y2
...). If FILLED is true the polygon will be filled with COLOR,
otherwise it will be outlined. If START and/or END are specified then
only the corresponding part of VERTICES is used as input."))

(defmethod draw-polygon ((vertices vector) &key filled (start 0) (end (length vertices)) (color *default-color*) (image *default-image*))
  (check-type start integer)
  (check-type end integer)
  (check-type image image)
  (let ((effective-length (- end start)))
    (unless (and (>= effective-length 6)
                 (evenp effective-length))
      (error "We need an even number of at least six vertices"))
    (with-safe-alloc (arr (allocate-foreign-object 'gd-point (/ effective-length 2))
                          (free-foreign-object arr))
      (with-color-argument
          (with-transformed-alternative
              (((aref vertices i) x-transformer)
               ((aref vertices (1+ i)) y-transformer))
            (loop for i from start below end by 2
                  for point-ptr = (deref-array arr '(:array gd-point) (/ (- i start) 2))
                  do (setf (get-slot-value point-ptr 'gd-point 'x)
                             (aref vertices i)
                           (get-slot-value point-ptr 'gd-point 'y)
                             (aref vertices (1+ i))))
            (funcall (if filled
                       #'gd-image-filled-polygon
                       #'gd-image-polygon)
                     (img image) arr (/ effective-length 2) color)
            vertices)))))

(defmethod draw-polygon ((vertices list) &key filled (start 0) (end (length vertices)) (color *default-color*) (image *default-image*))
  (check-type start integer)
  (check-type end integer)
  (check-type image image)
  (let ((effective-length (- end start)))
    (unless (and (>= effective-length 6)
                 (evenp effective-length))
      (error "We need an even number of at least six vertices"))
    (with-safe-alloc (arr (allocate-foreign-object 'gd-point (/ effective-length 2))
                          (free-foreign-object arr))
      (with-color-argument
          (with-transformed-alternative
              (((first x/y) x-transformer)
               ((second x/y) y-transformer))
            (loop for i below (- end start) by 2
                  ;; we don't use LOOP's destructuring capabilities here
                  ;; because of your simple WITH-TRANSFORMED-ALTERNATIVE
                  ;; macro which would get confused
                  for x/y on (nthcdr start vertices) by #'cddr
                  for point-ptr = (deref-array arr '(:array gd-point) (/ i 2))
                  do (setf (get-slot-value point-ptr 'gd-point 'x)
                             (first x/y)
                           (get-slot-value point-ptr 'gd-point 'y)
                             (second x/y)))
            (funcall (if filled
                       #'gd-image-filled-polygon
                       #'gd-image-polygon)
                     (img image) arr (/ effective-length 2) color)
            vertices)))))

(defun draw-filled-ellipse (center-x center-y width height &key (color *default-color*) (image *default-image*))
  "Draws a filled ellipse centered at \(CENTER-X, CENTER-Y) with width
WIDTH and height HEIGHT."
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((center-x x-transformer)
         (center-y y-transformer)
         (width w-transformer)
         (height h-transformer))
      (gd-image-filled-ellipse (img image) center-x center-y width height color)))
  (values center-x center-y width height))

(defun draw-filled-circle (center-x center-y radius &key (color *default-color*) (image *default-image*))
  "Draws a filled circle centered at \(CENTER-X, CENTER-Y) with radius
RADIUS."
  (draw-filled-ellipse center-x center-y (* 2 radius) (* 2 radius)
                :color color :image image)
  (values center-x center-y radius))

(defun draw-arc (center-x center-y width height start end &key straight-line center-connect filled (color *default-color*) (image *default-image*))
  "Draws a partial ellipse centered at \(CENTER-X, CENTER-Y) with
width WIDTH and height HEIGHT. The arc begins at angle START and ends
at angle END. If STRAIGHT-LINE is true the start and end points are
just connected with a straight line. If CENTER-CONNECT is true, they
are connected to the center \(which is useful to create 'pie
slices'). If FILLED is true the arc will be filled with COLOR,
otherwise it will be outlined."
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((center-x x-transformer)
         (center-y y-transformer)
         (width w-transformer)
         (height h-transformer)
         (start angle-transformer)
         (end angle-transformer))
      (cond ((not (or straight-line filled center-connect))
              (gd-image-arc (img image) center-x center-y width height start end color))
            (t
              (gd-image-filled-arc (img image) center-x center-y width height start end color
                                   (logior (if straight-line +gd-chord+ 0)
                                           (if filled 0 +gd-no-fill+)
                                           (if center-connect +gd-edged+ 0)))))))
  (values center-x center-y width height start end))

(defun fill-image (x y &key border (color *default-color*) (image *default-image*))
  "Floods a portion of the image IMAGE with the color COLOR beginning
at point \(X, Y) and extending into the surrounding region. If BORDER
is true it must be a color and the filling will stop at the specified
border color. Otherwise only points with the same color as the
starting point will be colored."
  (check-type border (or null integer))
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((x x-transformer)
         (y y-transformer))
      (if border
        (gd-image-fill-to-border (img image) x y border color)
        (gd-image-fill (img image) x y color))))
  (values x y))

(defun clipping-rectangle (&optional (image *default-image*))
  "Returns the clipping rectangle of IMAGE as a list of four
elements."
  (check-type image image)
  (with-transformed-alternative
      (((deref-pointer x1p) x-inv-transformer)
       ((deref-pointer y1p) y-inv-transformer)
       ((deref-pointer x2p) x-inv-transformer)
       ((deref-pointer y2p) y-inv-transformer))
    (with-foreign-object (x1p :int)
      (with-foreign-object (y1p :int)
        (with-foreign-object (x2p :int)
          (with-foreign-object (y2p :int)
            (gd-image-get-clip (img image) x1p y1p x2p y2p)
            (list (deref-pointer x1p :int)
                  (deref-pointer y1p :int)
                  (deref-pointer x2p :int)
                  (deref-pointer y2p :int))))))))

(defun (setf clipping-rectangle) (rectangle &optional (image *default-image*))
  "Sets the clipping rectangle of IMAGE where rectangle should be a
list \(X1 Y1 X2 Y2)."
  (check-type image image)
  (with-transformed-alternative
      (((first rectangle) x-transformer)
       ((second rectangle) y-transformer)
       ((third rectangle) x-transformer)
       ((fourth rectangle) y-transformer))
    (gd-image-set-clip (img image)
                       (first rectangle)
                       (second rectangle)
                       (third rectangle)
                       (fourth rectangle)))
  rectangle)

(defun clipping-rectangle* (&optional (image *default-image*))
  "Returns the clipping rectangle of IMAGE as four values."
  (check-type image image)
  (with-transformed-alternative
      (((deref-pointer x1p) x-inv-transformer)
       ((deref-pointer y1p) y-inv-transformer)
       ((deref-pointer x2p) x-inv-transformer)
       ((deref-pointer y2p) y-inv-transformer))
    (with-foreign-object (x1p :int)
      (with-foreign-object (y1p :int)
        (with-foreign-object (x2p :int)
          (with-foreign-object (y2p :int)
            (gd-image-get-clip (img image) x1p y1p x2p y2p)
            (values (deref-pointer x1p :int)
                    (deref-pointer y1p :int)
                    (deref-pointer x2p :int)
                    (deref-pointer y2p :int))))))))

(defun set-clipping-rectangle* (x1 y1 x2 y2 &optional (image *default-image*))
  "Sets the clipping rectangle of IMAGE to be the rectangle with upper
left corner \(X1, Y1) and lower right corner \(X2, Y2)."
  (check-type image image)
  (with-transformed-alternative
      ((x1 x-transformer)
       (y1 y-transformer)
       (x2 x-transformer)
       (y2 y-transformer))
    (gd-image-set-clip (img image) x1 y1 x2 y2))
  (values x1 y1 x2 y2))

(defmacro with-clipping-rectangle ((rectangle &key (image '*default-image*)) &body body)
  "Executes BODY with the clipping rectangle of IMAGE set to RECTANGLE
which should be a list \(X1 Y1 X2 Y2). The previous clipping rectangle
is guaranteed to be restored before the macro exits."
  ;; we rebind everything so we have left-to-right evaluation
  (with-rebinding (rectangle image)
    (with-unique-names (%x1 %y1 %x2 %y2)
      `(multiple-value-bind (,%x1 ,%y1 ,%x2 ,%y2)
           (without-transformations
             (clipping-rectangle* ,image))
         (unwind-protect
             (progn
               (setf (clipping-rectangle ,image) ,rectangle)
               ,@body)
           (without-transformations
             (set-clipping-rectangle* ,%x1 ,%y1 ,%x2 ,%y2 ,image)))))))

(defmacro with-clipping-rectangle* ((x1 y1 x2 y2 &key (image '*default-image*)) &body body)
  "Executes BODY with the clipping rectangle of IMAGE set to the
rectangle with upper left corner \(X1, Y1) and lower right corner
\(X2, Y2). The previous clipping rectangle is guaranteed to be
restored before the macro exits."
  ;; we rebind everything so we have left-to-right evaluation
  (with-rebinding (x1 y1 x2 y2 image)
    (with-unique-names (%x1 %y1 %x2 %y2)
      `(multiple-value-bind (,%x1 ,%y1 ,%x2 ,%y2)
           (without-transformations
             (clipping-rectangle* ,image))
         (unwind-protect
             (progn
               (set-clipping-rectangle* ,x1 ,y1 ,x2 ,y2 ,image)
               ,@body)
           (without-transformations
             (set-clipping-rectangle* ,%x1 ,%y1 ,%x2 ,%y2 ,image)))))))
