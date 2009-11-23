;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/transform.lisp,v 1.23 2009/11/23 17:05:39 edi Exp $

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

(defclass transformer ()
  ((image :initarg :image
          :reader image)
   (w-transformer :initarg :w-transformer
                  :reader w-transformer
                  :type function)
   (h-transformer :initarg :h-transformer
                  :reader h-transformer
                  :type function)
   (x-transformer :initarg :x-transformer
                  :reader x-transformer
                  :type function)
   (y-transformer :initarg :y-transformer
                  :reader y-transformer
                  :type function)
   (w-inv-transformer :initarg :w-inv-transformer
                      :reader w-inv-transformer
                      :type function)
   (h-inv-transformer :initarg :h-inv-transformer
                      :reader h-inv-transformer
                      :type function)
   (x-inv-transformer :initarg :x-inv-transformer
                      :reader x-inv-transformer
                      :type function)
   (y-inv-transformer :initarg :y-inv-transformer
                      :reader y-inv-transformer
                      :type function)
   (angle-transformer :initarg :angle-transformer
                      :reader angle-transformer
                      :type function))
  (:documentation "Class used internally for WITH-TRANSFORMATION
macro."))

(defmacro without-transformations (&body body)
  "Executes BODY without any transformations applied."
  `(let (*transformers*)
    ,@body))

(declaim (inline round-to-c-int))
(defun round-to-signed-byte-32 (x)
  "Like ROUND but make sure result isn't longer than 32 bits."
  (mod (round x) +most-positive-unsigned-byte-32+))

(defmacro with-transformation ((&key (x1 0 x1set) (x2 0 x2set) (width 0 wset)
                                     (y1 0 y1set) (y2 0 y2set) (height 0 hset)
                                     reverse-x reverse-y (radians t) (image '*default-image*))
                               &body body)
  "Executes BODY such that all points and width/height data are
subject to a simple affine transformation defined by the keyword
parameters. The new x-axis of IMAGE will start at X1 and end at X2 and
have length WIDTH. The new y-axis of IMAGE will start at Y1 and end at
Y2 and have length HEIGHT. In both cases it suffices to provide two of
the three values - if you provide all three they have to match. If
REVERSE-X is false the x-axis will be oriented as usual in Cartesian
coordinates, otherwise its direction will be reversed. The same
applies to REVERSE-Y, of course. If RADIANS is true angles inside of
BODY will be assumed to be provided in radians, otherwise in degrees."
  (with-rebinding (x1 x2 width y1 y2 height reverse-x reverse-y radians image)
    (with-unique-names (image-width image-height
                        stretch-x stretch-y
                        w-transformer h-transformer
                        x-transformer y-transformer
                        w-inv-transformer h-inv-transformer
                        x-inv-transformer y-inv-transformer
                        angle-transformer)
      ;; rebind for thread safety
      `(let ((*transformers* *transformers*))
         (macrolet ((checkargs (a1 a1set a2 a2set aspan aspanset c lbl)
                      `(progn
                         (cond ((and ,a1set ,a2set) (setq ,aspan (- ,a2 ,a1)))
                               ((and ,a1set ,aspanset) (setq ,a2 (+ ,a1 ,aspan)))
                               ((and ,a2set ,aspanset) (setq ,a1 (- ,a2 ,aspan)))
                               (t (error "Two of ~c1, ~:*~c2, or ~a must be provided." ,c ,lbl)))
                         (unless (> ,aspan 0)
                           (error "~c1 must be smaller than ~:*~c2." ,c))
                         (unless (< (abs (/ (- ,a2 (+ ,a1 ,aspan)) ,aspan)) 1.e-5)
                           (error "~c1, ~:*~c2, and ~a don't match.  Try to provide just two of the three arguments." ,c ,lbl)))))
           (checkargs ,x1 ,x1set ,x2 ,x2set ,width ,wset #\x "width")
           (checkargs ,y1 ,y1set ,y2 ,y2set ,height ,hset #\y "height"))
         (multiple-value-bind (,image-width ,image-height)
            (without-transformations
             (image-size ,image))
          (let* ((,stretch-x (/ ,image-width ,width))
                 (,stretch-y (/ ,image-height ,height))
                 (,w-transformer (lambda (w)
                                  (round-to-signed-byte-32
                                   (* w ,stretch-x))))
                 (,w-inv-transformer (lambda (w)
                                       (/ w ,stretch-x)))
                 (,h-transformer (lambda (h)
                                  (round-to-signed-byte-32
                                   (* h ,stretch-y))))
                 (,h-inv-transformer (lambda (h)
                                       (/ h ,stretch-y)))
                 (,x-transformer (if ,reverse-x
                                  (lambda (x)
                                    (round-to-signed-byte-32
                                     (* (- ,x2 x) ,stretch-x)))
                                  (lambda (x)
                                    (round-to-signed-byte-32
                                     (* (- x ,x1) ,stretch-x)))))
                 (,x-inv-transformer (if ,reverse-x
                                       (lambda (x)
                                         (- ,x2 (/ x ,stretch-x)))
                                       (lambda (x)
                                         (+ ,x1 (/ x ,stretch-x)))))
                 (,y-transformer (if ,reverse-y
                                  (lambda (y)
                                    (round-to-signed-byte-32
                                     (* (- y ,y1) ,stretch-y)))
                                  (lambda (y)
                                    (round-to-signed-byte-32
                                     (* (- ,y2 y) ,stretch-y)))))
                 (,y-inv-transformer (if ,reverse-y
                                       (lambda (y)
                                         (+ ,y1 (/ y ,stretch-y)))
                                       (lambda (y)
                                         (- ,y2 (/ y ,stretch-y)))))
                 (,angle-transformer (cond (,radians
                                             (lambda (angle)
                                               (round-to-signed-byte-32
                                                (* angle
                                                   +radians-to-degree-factor+))))
                                           (t
                                             #'identity))))
            (push (make-instance 'transformer
                                 :image ,image
                                 :w-transformer ,w-transformer
                                 :h-transformer ,h-transformer
                                 :x-transformer ,x-transformer
                                 :y-transformer ,y-transformer
                                 :w-inv-transformer ,w-inv-transformer
                                 :h-inv-transformer ,h-inv-transformer
                                 :x-inv-transformer ,x-inv-transformer
                                 :y-inv-transformer ,y-inv-transformer
                                 :angle-transformer ,angle-transformer)
                  *transformers*)
            (unwind-protect
              (progn
                ,@body)
              (pop *transformers*))))))))

(defmacro with-transformed-alternative ((&rest transformations) &body body)
  "Internal macro used to make functions
transformation-aware. TRANSFORMATION is a list of (EXPR
TRANSFORMATION) pairs where each EXPR will be replaced by the
transformation denoted by TRANSFORMATION."
  (with-unique-names (transformer)
    (let ((transformations-alist
            (loop for (expr transformation) in transformations
                  collect `(,expr . (funcall (,transformation ,transformer) ,expr)))))
      ;; note that we always use the name 'IMAGE' - no problem because
      ;; this is a private macro
      `(let ((,transformer (find image *transformers* :key #'image)))
        (cond (,transformer
                ,(sublis transformations-alist
                         `(progn ,@body)
                         :test #'equal))
              (t (progn
                   ,@body)))))))
