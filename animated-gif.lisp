;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/gd-uffi.lisp,v 1.33 2009/11/23 17:05:39 edi Exp $

;;; Copyright (c) 2012, Hans Huebner.  All rights reserved.

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

;; GIF animation context, contains plist with animation parameters.
(defvar *current-animation*)

(defun add-image-to-animation (image
                               &key
                                 (local-color-map-p nil local-color-map-provided-p)
                                 (left-offset 0)
                                 (top-offset 0)
                                 (delay nil delay-provided-p)
                                 (disposal :none)
                                 last-image
                                 (animation *current-animation*))
  (gd-image-gif-anim-add (img image)
                         (getf animation :anim-pointer)
                         (if local-color-map-provided-p
                             (if local-color-map-p 1 0)
                             (if (getf animation :global-color-map-p) 0 1))
                         left-offset
                         top-offset
                         (or (when delay-provided-p
                               delay)
                             (getf animation :default-delay)
                             (error "no delay specified and no default-delay set in animation context"))
                         (ecase disposal
                           (:none +gd-disposal-none+)
                           (:restore-background +gd-disposal-restore-background+)
                           (:restore-previous +gd-disposal-restore-previous+))
                         (when last-image
                           (img last-image))))

(defmacro with-animated-gif ((filename
                              &key
                                (background-image '*default-image*)
                                global-color-map-p
                                (loop-count -1)
                                (animation '*current-animation*)
                                default-delay)
                             &body body)
  `(let ((,animation (list :anim-pointer (gd-image-gif-anim-begin-wrap (img ,background-image)
                                                                       ,filename
                                                                       (if ,global-color-map-p 1 0)
                                                                       ,loop-count)
                           :global-color-map-p ,global-color-map-p
                           :default-delay ,default-delay)))
     (unwind-protect
          (progn ,@body)
       (gd-image-gif-anim-end-wrap (getf ,animation :anim-pointer)))))

