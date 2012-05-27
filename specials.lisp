;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/specials.lisp,v 1.30 2009/11/23 17:05:39 edi Exp $

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

(in-package #:cl-gd)

(defvar *default-image* nil
  "The default image. This special variable is usually bound by
WITH-IMAGE or WITH-IMAGE-FROM-FILE.")

(defvar *default-color* nil
  "The default color. This special variable is usually bound by
WITH-COLOR.")

(defvar *default-font* nil
  "The default font. This special variable is usually bound by
WITH-FONT.")

(defstruct (image
             (:conc-name nil)
             (:constructor make-image (img))
             (:copier nil))
  img)

(defstruct (brush
             (:include image)
             (:constructor %make-brush (img))
             (:copier nil)))

(defun make-brush (image)
  (%make-brush (img image)))

(defstruct (tile
             (:include image)
             (:constructor %make-tile (img))
             (:copier nil)))

(defun make-tile (image)
  (%make-tile (img image)))

(defstruct (anti-aliased-color
             (:conc-name nil)
             (:constructor %make-anti-aliased (color do-not-blend))
             (:copier nil))
  color do-not-blend)

(defun make-anti-aliased (color &optional do-not-blend)
  (%make-anti-aliased color do-not-blend))

;; the following variable will be initialized in "gd-uffi.lisp"
(defvar *null-image* nil
  "A 'null' image which might be useful for DRAW-FREETYPE-STRING.")

(defconstant +max-colors+ 256
  "Maximum number of colors for palette-based images.")

(defconstant +gd-chord+ 1
  "Used internally by GD-FILLED-ARC")
(defconstant +gd-no-fill+ 2
  "Used internally by GD-FILLED-ARC")
(defconstant +gd-edged+ 4
  "Used internally by GD-FILLED-ARC")

(defconstant +brushed+ -3
  "Special 'color' for lines drawn with brush.")
(defconstant +styled+ -2
  "Special 'color' for styled lines.")
(defconstant +styled-brushed+ -4
  "Special 'color' for lines drawn with styled brush.")
(defconstant +transparent+ -6
  "Special 'color' used in GD function 'gdImageSetStyle' for transparent color.")
(defconstant +tiled+ -5
  "Special fill 'color' used for tiles.")
(defconstant +anti-aliased+ -7
  "Special 'color' for anti-aliased lines.")

(defconstant +gd-ftex-linespace+ 1
  "Indicate line-spacing for FreeType library.")

(defconstant +gd-cmp-image+ 1
  "Images will appear different when displayed.")
(defconstant +gd-cmp-num-colors+ 2
  "Number of colors in palette differ.")
(defconstant +gd-cmp-color+ 4
  "Image colors differ.")
(defconstant +gd-cmp-size-x+ 8
  "Image widths differ.")
(defconstant +gd-cmp-size-y+ 16
  "Image heights differ.")
(defconstant +gd-cmp-transparent+ 32
  "Transparent color is different.")
(defconstant +gd-cmp-background+ 64
  "Background color is different.")
(defconstant +gd-cmp-interlace+ 128
  "Interlace settings are different.")
(defconstant +gd-cmp-true-color+ 256
  "One image is a true-color image, the other one is palette-based.")

(defconstant +gd-disposal-unknown+ 0)
(defconstant +gd-disposal-none+ 1)
(defconstant +gd-disposal-restore-background+ 2)
(defconstant +gd-disposal-restore-previous+ 2)

(defvar *shared-library-directories*
  `(,(namestring (make-pathname :name nil
                                :type nil
                                :version :newest
                                :defaults cl-gd.system:*cl-gd-directory*))
     "/usr/local/lib/"
     "/usr/lib/"
     "/usr/lib/cl-gd/"
     "/cygwin/usr/local/lib/"
     "/cygwin/usr/lib/")
  "A list of directories where UFFI tries to find cl-gd-glue.so")
(defvar *shared-library-types* '("so" "dll" "dylib")
  "The list of types a shared library can have. Used when looking for
cl-gd-glue.so")
(defvar *shared-library-drive-letters* '("C" "D" "E" "F" "G")
  "The list of drive letters \(used by Wintendo) used when looking for
cl-gd-glue.dll.")

(defvar *gd-supporting-libraries* '("c" "gd" "png" "z" "jpeg" "freetype" "iconv" "m")
  "The libraries which are needed by cl-gd-glues.so \(and GD
itself). Only needed for Python-based Lisps like CMUCL, SBCL, or
SCL.")

(defconstant +radians-to-degree-factor+ (/ 360 (* 2 pi))
  "Factor to convert from radians to degrees.")

(defvar *transformers* nil
  "Stack of currently active transformer objects.")

(defconstant +most-positive-unsigned-byte-32+
  (1- (expt 2 31))
  "Name says it all...")

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-gd/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-gd
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))