;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/misc.lisp,v 1.16 2009/11/23 17:05:39 edi Exp $

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

(defun interlacedp (&optional (image *default-image*))
  "Returns whether IMAGE will be stored in an interlaced fashion."
  (check-type image image)
  (not (zerop (gd-image-get-interlaced (img image)))))

(defun (setf interlacedp) (interlaced &optional (image *default-image*))
  "Sets whether IMAGE will be stored in an interlaced fashion."
  (check-type image image)
  (gd-image-interlace (img image) (if interlaced 1 0))
  interlaced)

(defun differentp (image1 image2)
  "Returns false if the two images won't appear different when
displayed. Otherwise the return value is a list of keywords describing
the differences between the images."
  (check-type image1 image)
  (check-type image2 image)
  (let ((result (gd-image-compare (img image1) (img image2))))
    (cond ((zerop (logand +gd-cmp-image+ result))
            nil)
          (t
            (loop for (gd-flag keyword) in `((,+gd-cmp-num-colors+
                                              :different-number-of-colors)
                                             (,+gd-cmp-color+
                                              :different-colors)
                                             (,+gd-cmp-size-x+
                                              :different-widths)
                                             (,+gd-cmp-size-y+
                                              :different-heights)
                                             (,+gd-cmp-transparent+
                                              :different-transparent-colors)
                                             (,+gd-cmp-background+
                                              :different-background-colors)
                                             (,+gd-cmp-interlace+
                                              :different-interlace-settings)
                                             (,+gd-cmp-true-color+
                                              :true-color-versus-palette-based))
                  when (plusp (logand gd-flag result))
                  collect keyword)))))

(defun copy-image (source destination
                   source-x source-y
                   dest-x dest-y
                   width height
                   &key resample
                        rotate angle
                        resize dest-width dest-height
                        merge merge-gray)
  "Copies \(a part of) image SOURCE into image DESTINATION. Copies the
rectangle with the upper left corner \(SOURCE-X,SOURCE-Y) and size
WIDTH x HEIGHT to the rectangle with the upper left corner
\(DEST-X,DEST-Y).

If RESAMPLE is true pixel colors will be smoothly interpolated. If
RESIZE is true the copied rectangle will be strechted or shrinked so
that its size is DEST-WIDTH x DEST-HEIGHT. If ROTATE is true the image
will be rotated by ANGLE. In this particular case DEST-X and DEST-Y
specify the CENTER of the copied image rather than its upper left
corner! If MERGE is true it has to be an integer in the range 0-100
and the two images will be 'merged' by the amount specified. If MERGE
is 100 then the source image will simply be copied. If instead
MERGE-GRAY is true the hue of the source image is preserved by
converting the destination area to gray pixels before merging.

The keyword options RESAMPLE, ROTATE, RESIZE, MERGE, and MERGE-GRAY
are mutually exclusive \(with the exception of RESAMPLE and
RESIZE). ANGLE is assumed to be specified in degrees if it's an
integer, and in radians otherwise."
  (check-type source image)
  (check-type destination image)
  (check-type source-x integer)
  (check-type source-y integer)
  (unless rotate
    (check-type dest-x integer)
    (check-type dest-y integer))
  (check-type width integer)
  (check-type height integer)
  (check-type angle (or null number))
  (check-type dest-width (or null integer))
  (check-type dest-height (or null integer))
  (check-type merge (or null (integer 0 100)))
  (check-type merge-gray (or null (integer 0 100)))
  (when (and merge merge-gray)
    (error "You can't specify MERGE and MERGE-GRAY at the same time."))
  (when (and (or merge merge-gray)
             (or resample rotate resize))
    (error "MERGE and MERGE-GRAY can't be combined with RESAMPLE, ROTATE, or RESIZE."))
  (when (and (or dest-width dest-height)
             (not resize))
    (error "Use RESIZE if you want to specify DEST-WIDTH or DEST-HEIGHT"))
  (when (and resize
             (not (or dest-width dest-height)))
    (error "Please specify DEST-WIDTH and DEST-HEIGHT together with RESIZE."))
  (when (and angle
             (not rotate))
    (error "Use ROTATE if you want to specify ANGLE."))
  (when (and rotate
             (not angle))
    (error "Please specify ANGLE together with ROTATE."))
  (when (and rotate
             (or resample resize))
    (error "ROTATE can't be used together with RESAMPLE or RESIZE."))
  (cond ((and resample resize)
          (gd-image-copy-resampled (img destination) (img source)
                                   dest-x dest-y source-x source-y
                                   dest-width dest-height width height))
        (resample
          (gd-image-copy-resampled (img destination) (img source)
                                   dest-x dest-y source-x source-y
                                   width height width height))
        ((and rotate (integerp angle))
          (gd-image-copy-rotated (img destination) (img source)
                                 (coerce dest-x 'double-float)
                                 (coerce dest-y 'double-float)
                                 source-x source-y width height angle))
        (rotate
          (gd-image-copy-rotated (img destination) (img source)
                                 (coerce dest-x 'double-float)
                                 (coerce dest-y 'double-float)
                                 source-x source-y width height
                                 (round (* angle +radians-to-degree-factor+))))
        (resize
          (gd-image-copy-resized (img destination) (img source)
                                 dest-x dest-y source-x source-y
                                 dest-width dest-height width height))
        (merge
          (gd-image-copy-merge (img destination) (img source)
                               dest-x dest-y source-x source-y
                               width height merge))
        (merge-gray
          (gd-image-copy-merge-gray (img destination) (img source)
                                    dest-x dest-y source-x source-y
                                    width height merge-gray))
        (t
          (gd-image-copy (img destination) (img source) dest-x dest-y
                         source-x source-y width height)))
  destination)
          
(defun copy-palette (source destination)
  "Copies palette of image SOURCE to image DESTINATION attempting to
match the colors in the target image to the colors in the source
palette."
  (check-type source image)
  (check-type destination image)
  (gd-image-palette-copy (img destination) (img source))
  destination)

(defun true-color-to-palette (&key dither (colors-wanted 256) (image *default-image*))
  "Converts the true color image IMAGE to a palette-based image using
a high-quality two-pass quantization routine. If DITHER is true, the
image will be dithered to approximate colors better, at the expense of
some obvious \"speckling.\" COLORS-WANTED can be any positive integer
up to 256 \(which is the default). If the original source image
includes photographic information or anything that came out of a JPEG,
256 is strongly recommended. 100% transparency of a single transparent
color in the original true color image will be preserved. There is no
other support for preservation of alpha channel or transparency in the
destination image."
  (check-type image image)
  (check-type colors-wanted (integer 0 256))
  (gd-image-true-color-to-palette (img image)
                                  (if dither 1 0)
                                  colors-wanted)
  image)

(defmacro do-rows ((y-var &optional (image '*default-image*)) &body body)
  (with-rebinding (image)
    (with-unique-names (img width height true-color-p raw-pixels row x-var inner-body)
      `(let* ((,img (img ,image))
              (,width (gd-image-get-sx ,img))
              (,height (gd-image-get-sy ,img))
              (,true-color-p (true-color-p ,image)))
        (declare (fixnum ,width ,height))
        (cond (,true-color-p
                (let ((,raw-pixels (get-slot-value ,img 'gd-image 't-pixels)))
                  (declare (type t-pixels-array ,raw-pixels))
                  (dotimes (,y-var ,height)
                    (let ((,row (deref-array ,raw-pixels '(:array (* :int)) ,y-var)))
                      (declare (type t-pixels-row ,row))
                      (macrolet ((do-pixels-in-row ((,x-var) &body ,inner-body)
                                   `(dotimes (,,x-var ,',width)
                                     (macrolet ((raw-pixel ()
                                                  `(deref-array ,',',row '(:array :int) ,',,x-var)))
                                       (locally
                                         ,@,inner-body)))))
                        (locally
                          ,@body))))))
              (t 
                (let ((,raw-pixels (get-slot-value ,img 'gd-image 'pixels)))
                  (declare (type pixels-array ,raw-pixels))
                  (dotimes (,y-var ,height)
                    (let ((,row (deref-array ,raw-pixels '(:array (* :unsigned-byte)) ,y-var)))
                      (declare (type pixels-row ,row))
                      (macrolet ((do-pixels-in-row ((,x-var) &body ,inner-body)
                                   `(dotimes (,,x-var ,',width)
                                     (macrolet ((raw-pixel ()
                                                  `(deref-array ,',',row '(:array :unsigned-byte) ,',,x-var)))
                                       (locally
                                         ,@,inner-body)))))
                        (locally
                          ,@body)))))))))))

(defmacro do-pixels ((&optional (image '*default-image*)) &body body)
  (with-unique-names (x y)
    `(do-rows (,y ,image)
      (do-pixels-in-row (,x)
        ,@body))))