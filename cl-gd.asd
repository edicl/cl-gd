;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/cl-gd.asd,v 1.7 2005/03/09 14:17:56 edi Exp $

;;; Copyright (c) 2003-2005, Dr. Edmund Weitz.  All rights reserved.

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

(in-package #:cl-user)

(defpackage #:cl-gd.system
  (:use #:cl
        #:asdf)
  (:export #:*cl-gd-directory*))

(in-package #:cl-gd.system)

(defparameter *cl-gd-directory*
  (make-pathname :name nil :type nil :version nil
                 :defaults (parse-namestring *load-truename*)))

(defsystem #:cl-gd
    :components ((:file "packages")
                 (:file "util" :depends-on ("packages"))
                 (:file "specials" :depends-on ("util"))
                 (:file "init" :depends-on ("specials"))
                 (:file "gd-uffi" :depends-on ("init"))
                 (:file "transform" :depends-on ("gd-uffi"))
                 (:file "images" :depends-on ("transform"))
                 (:file "colors-aux" :depends-on ("transform"))
                 (:file "colors" :depends-on ("transform"))
                 (:file "drawing" :depends-on ("transform" "colors-aux"))
                 (:file "strings" :depends-on ("transform" "colors-aux"))
                 (:file "misc" :depends-on ("transform")))
    :depends-on (:uffi))
