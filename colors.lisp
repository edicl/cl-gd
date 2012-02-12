;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/colors.lisp,v 1.26 2009/11/23 17:05:38 edi Exp $

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

(defmacro with-default-color ((color) &body body)
  "Executes BODY with *DEFAULT-COLOR* bound to COLOR so that you don't
have to provide the COLOR keyword/optional argument to drawing
functions."
  `(let ((*default-color* ,color))
    ,@body))

(defun allocate-color (red green blue &key alpha (errorp t) (image *default-image*))
  "Finds the first available color index in the image IMAGE specified,
sets its RGB values to those requested \(255 is the maximum for each),
and returns the index of the new color table entry, or an RGBA value
in the case of a true color image. In either case you can then use the
returned value as a COLOR parameter to drawing functions. When
creating a new palette-based image, the first time you invoke this
function you are setting the background color for that image. If ALPHA
\(not greater than 127) is provided, an RGBA color will always be
allocated. If all +GD-MAX-COLORS+ have already been allocated this
function will, depending on the value of ERRORP, either raise an error
or return NIL."
  (check-type red integer)
  (check-type green integer)
  (check-type blue integer)
  (check-type alpha (or null integer))
  (check-type image image)
  (let ((result
          (if alpha
            (gd-image-color-allocate-alpha (img image) red green blue alpha)
            (gd-image-color-allocate (img image) red green blue))))
    (cond ((and errorp
                (= result -1))
            (error "Can't allocate color"))
          ((= result -1)
            nil)
          (t
            result))))

(defun deallocate-color (color &optional (image *default-image*))
  "Marks the specified color COLOR as being available for reuse. No
attempt will be made to determine whether the color index is still in
use in the image IMAGE."
  (check-type color integer)
  (check-type image image)
  (gd-image-color-deallocate (img image) color))

(defun transparent-color (&optional (image *default-image*))
  "Returns the transparent color of IMAGE \(or NIL if there is none)."
  (check-type image image)
  (gd-image-get-transparent (img image)))

(defun (setf transparent-color) (color &optional (image *default-image*))
  "Makes COLOR the transparent color of IMAGE. If COLOR is NIL the
image won't have a transparent color. Note that JPEG images don't
support transparency."
  (check-type color (or null integer))
  (check-type image image)
  (gd-image-color-transparent (img image) (or color -1))
  color)

(defun true-color-p (&optional (image *default-image*))
  "Returns true iff IMAGE is a true color image."
  (check-type image image)
  (not (zerop (get-slot-value (img image) 'gd-image 'true-color))))

(defun number-of-colors (&key (image *default-image*))
  "Returns the number of color allocated in IMAGE. Returns NIL if
IMAGE is a true color image."
  (check-type image image)
  (if (true-color-p image)
    nil
    (get-slot-value (img image) 'gd-image 'colors-total)))

(defun find-color (red green blue &key alpha exact hwb resolve (image *default-image*))
  "Tries to find and/or allocate a color from IMAGE's color
palette. If EXACT is true, the color will only be returned if it is
already allocated. If EXACT is NIL, a color which is 'close' to the
color specified by RED, GREEN, and BLUE \(and probably ALPHA) might be
returned \(unless there aren't any colors allocated in the image
yet). If HWB is true, the 'closeness' will be determined by hue,
whiteness, and blackness, otherwise by the Euclidian distance of the
RGB values. If RESOLVE is true a color \(probably a new one) will
always be returned, otherwise the result of this function might be
NIL. If ALPHA \(not greater than 127) is provided, an RGBA color (or
NIL) will be returned.

ALPHA, EXACT, and HWB are mutually exclusive. RESOLVE can't be used
together with EXACT or HWB."
  (check-type red integer)
  (check-type green integer)
  (check-type blue integer)
  (check-type alpha (or null integer))
  (check-type image image)
  (when (< 1 (count-if #'identity (list alpha exact hwb)))
    (error "You can't specify two of ALPHA, EXACT, and HWB at the same
time"))
  (when (and hwb resolve)
    (error "You can't specify HWB and RESOLVE at the same time"))
  (when (and exact resolve)
    (error "You can't specify EXACT and RESOLVE at the same time"))
  (let ((result
          (cond ((and resolve alpha)
                  (gd-image-color-resolve-alpha (img image) red green blue alpha))
                (resolve
                  (gd-image-color-resolve (img image) red green blue))
                (alpha
                  (gd-image-color-closest-alpha (img image) red green blue alpha))
                (exact
                  (gd-image-color-exact (img image) red green blue))
                (hwb
                  (gd-image-color-closest-hwb (img image) red green blue))
                (t
                  (gd-image-color-closest (img image) red green blue)))))
    (if (= result -1)
      nil
      result)))

(defun thickness (&optional (image *default-image*))
  "Gets the width of lines drawn by the drawing functions. Note that
this is measured in pixels and is NOT affected by
WITH-TRANSFORMATION."
  (check-type image image)
  (get-slot-value (img image) 'gd-image 'thick))

(defun (setf thickness) (thickness &optional (image *default-image*))
  "Sets the width of lines drawn by the drawing functions. Note that
THICKNESS is measured in pixels and is NOT affected by
WITH-TRANSFORMATION."
  (check-type thickness integer)
  (check-type image image)
  (gd-image-set-thickness (img image) thickness)
  thickness)

(defmacro with-thickness ((thickness &key (image '*default-image*)) &body body)
  "Executes BODY with the current line width of IMAGE set to
THICKNESS. The image's previous line width is guaranteed to be
restored before the macro exits. Note that the line width is measured
in pixels and is not affected by WITH-TRANSFORMATION."
  (with-unique-names (old-thickness)
    ;; we rebind everything so we have left-to-right evaluation
    (with-rebinding (thickness image)
      `(let ((,old-thickness (thickness ,image)))
         (unwind-protect
             (progn
               (setf (thickness ,image) ,thickness))
           ,@body)
         (setf (thickness ,image) ,old-thickness)))))

(defun alpha-blending-p (&optional (image *default-image*))
  "Returns whether pixels drawn on IMAGE will be copied literally
including alpha channel information \(return value is false) or if
their alpha channel information will determine how much of the
underlying color will shine through \(return value is true). This is
only meaningful for true color images."
  (check-type image image)
  (not (zerop (get-slot-value (img image) 'gd-image 'alpha-blending-flag))))

(defun (setf alpha-blending-p) (blending &optional (image *default-image*))
  "Determines whether pixels drawn on IMAGE will be copied literally
including alpha channel information \(if BLENDING is false) or if
their alpha channel information will determine how much of the
underlying color will shine through \(if BLENDING is true). This is
only meaningful for true color images."
  (check-type image image)
  (gd-image-alpha-blending (img image) (if blending 1 0))
  blending)

(defun save-alpha-p (&optional (image *default-image*))
  "Returns whether PNG images will be saved with full alpha channel
information."
  (check-type image image)
  (not (zerop (get-slot-value (img image) 'gd-image 'save-alpha-flag))))

(defun (setf save-alpha-p) (save &optional (image *default-image*))
  "Determines whether PNG images will be saved with full alpha channel
information."
  (check-type image image)
  (gd-image-save-alpha (img image) (if save 1 0))
  save)

(defun color-component (component color &key (image *default-image*))
  "Returns the specified color component of COLOR. COMPONENT can be
one of :RED, :GREEN, :BLUE, and :ALPHA."
  (check-type color integer)
  (check-type image image)  
  (funcall (ecase component
             ((:red) #'gd-image-get-red)
             ((:green) #'gd-image-get-green)
             ((:blue) #'gd-image-get-blue)
             ((:alpha) #'gd-image-get-alpha))
           (img image)
           color))

(defun color-components (color &key (image *default-image*))
  "Returns a list of the color components of COLOR. The
components are in the order red, green, blue, alpha."
  (mapcar #'(lambda (c) (color-component c color :image image))
          '(:red :green :blue :alpha)))

(defun find-color-from-image (color source-image &key alpha exact hwb
                              resolve (image *default-image*))
  "Returns the color in IMAGE corresponding to COLOR in
SOURCE-IMAGE. The keyword parameters are passed to FIND-COLOR."
  (let ((red (color-component :red color
                              :image source-image))
        (blue (color-component :blue color
                               :image source-image))
        (green (color-component :green color
                                :image source-image))
        (alpha (when alpha
                 (color-component :alpha color
                                  :image source-image))))
    (find-color red green blue
                :alpha alpha
                :exact exact
                :hwb hwb
                :resolve resolve
                :image image)))
