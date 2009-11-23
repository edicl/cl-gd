;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/colors-aux.lisp,v 1.13 2009/11/23 17:05:38 edi Exp $

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

(defun current-brush (&optional (image *default-image*))
  "Returns the GD image which is the current brush of IMAGE \(or NIL
if there is no current brush)."
  (check-type image image)
  (let ((brush (get-slot-value (img image) 'gd-image 'brush)))
    (if (null-pointer-p brush)
      nil
      brush)))

(defun (setf current-brush) (brush &optional (image *default-image*))
  "Sets BRUSH \(which must be a GD image) to be the current brush
for IMAGE."
  (check-type brush image)
  (check-type image image)
  (gd-image-set-brush (img image) (img brush))
  brush)

(defun current-tile (&optional (image *default-image*))
  "Returns the GD image which is the current tile of IMAGE \(or NIL
if there is no current tile)."
  (check-type image image)
  (let ((tile (get-slot-value (img image) 'gd-image 'tile)))
    (if (null-pointer-p tile)
      nil
      tile)))

(defun (setf current-tile) (tile &optional (image *default-image*))
  "Sets TILE \(which must be a GD image) to be the current tile
for IMAGE."
  (check-type tile (or image null))
  (check-type image image)
  (gd-image-set-tile (img image) (img tile))
  tile)

(defun current-style (&optional (image *default-image*))
  "Returns the current style of IMAGE as a list."
  (check-type image image)
  (let ((style-length (get-slot-value (img image) 'gd-image 'style-length))
        (style (get-slot-value (img image) 'gd-image 'style)))
    (loop for i below style-length
          collect (let ((color (deref-array style '(:array :int) i)))
                    (if (= color +transparent+)
                      nil
                      color)))))

(defun current-style* (&key (image *default-image*))
  "Returns the current style of IMAGE as an array."
  (check-type image image)
  (let ((style-length (get-slot-value (img image) 'gd-image 'style-length))
        (style (get-slot-value (img image) 'gd-image 'style)))
    (loop with result = (make-array style-length)
          for i below style-length
          do (setf (aref result i)
                     (let ((color (deref-array style '(:array :int) i)))
                       (if (= color +transparent+)
                         nil
                         color)))
          finally (return result))))

(defgeneric (setf current-style) (style &optional image)
  (:documentation "Sets STYLE to be the current drawing style for
IMAGE. STYLE can be a LIST or a VECTOR. Each element of STYLE is
either a color or NIL \(for transparent pixels)."))

(defmethod (setf current-style) ((style list) &optional (image *default-image*))
  (check-type image image)
  (let ((length (length style)))
    (with-safe-alloc (c-style (allocate-foreign-object :int length)
                              (free-foreign-object c-style))
      (loop for color in style
            for i from 0
            do (setf (deref-array c-style '(:array :int) i)
                       (typecase color
                         (null +transparent+)
                         (integer color)
                         (t 1))))
      (gd-image-set-style (img image) c-style length)
      style)))

(defmethod (setf current-style) ((style vector) &optional (image *default-image*))
  (check-type image image)
  (let ((length (length style)))
    (with-safe-alloc (c-style (allocate-foreign-object :int length)
                              (free-foreign-object c-style))
      (loop for color across style
            for i from 0
            do (setf (deref-array c-style '(:array :int) i)
                       (typecase color
                         (null +transparent+)
                         (integer color)
                         (t 1))))
      (gd-image-set-style (img image) c-style length)
      style)))

(defun set-anti-aliased (color do-not-blend &optional (image *default-image*))
  "Set COLOR to be the current anti-aliased color of
IMAGE. DO-NOT-BLEND \(if provided) is the background color
anti-aliased lines stand out against clearly."
  (check-type color integer)
  (check-type do-not-blend (or integer null))
  (check-type image image)
  (gd-image-set-anti-aliased-do-not-blend (img image)
                                          color
                                          (or do-not-blend -1)))

(defun resolve-c-color (color image)
  "Accepts a CL-GD 'color' COLOR and returns the corresponding
argument for GD, modifying internal slots of IMAGE if needed."
  (etypecase color
    (brush
      (setf (current-brush image) color)
      +brushed+)
    (tile
      (setf (current-tile image) color)
      +tiled+)
    ((cons brush (or vector list))
      (setf (current-brush image) (car color)
            (current-style image) (cdr color))
      +styled-brushed+)
    (anti-aliased-color
      (set-anti-aliased (color color)
                        (do-not-blend color)
                        image)
      +anti-aliased+)
    ((or vector list)
      (setf (current-style image) color)
      +styled+)
    (integer
      color)))

(defmacro with-color-argument (&body body)
  "Internal macro used to give correct color arguments to enclosed
foreign functions. Assumes fixed names COLOR and IMAGE."
  (with-unique-names (c-color-arg)
    `(let ((,c-color-arg (resolve-c-color color image)))
      ,@(sublis (list (cons 'color c-color-arg))
                body :test #'eq))))
