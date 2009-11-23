;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/images.lisp,v 1.34 2009/11/23 17:05:39 edi Exp $

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

(defun create-image (width height &optional true-color)
  "Allocates and returns a GD image structure with size WIDTH x
HEIGHT. Creates a true color image if TRUE-COLOR is true. You are
responsible for destroying the image after you're done with it. It is
advisable to use WITH-IMAGE instead."
  (check-type width integer)
  (check-type height integer)
  (let ((image-ptr
          (if true-color
            (gd-image-create-true-color width height)
            (gd-image-create width height))))
    (when (null-pointer-p image-ptr)
      (error "Could not allocate image of size ~A x ~A" width height))
    (let ((image (make-image image-ptr)))
      image)))

(defun destroy-image (image)
  "Destroys \(deallocates) IMAGE which has been created by
CREATE-IMAGE, CREATE-IMAGE-FROM-FILE, or CREATE-IMAGE-FROM-GD2-PART."
  (check-type image image)
  (gd-image-destroy (img image))
  nil)

(defmacro with-default-image ((image) &body body)
  "Executes BODY with *DEFAULT-IMAGE* bound to IMAGE so that you don't
have to provide the IMAGE keyword/optional argument to CL-GD
functions."
  `(let ((*default-image* ,image))
    ,@body))

(defmacro with-image ((name width height &optional true-color) &body body)
  "Creates an image with size WIDTH x HEIGHT, and executes BODY with
the image bound to NAME. If TRUE-COLOR is true, creates a true color
image. The image is guaranteed to be destroyed before this macro
exits."
  ;; we rebind everything so we have left-to-right evaluation
  (with-rebinding (width height true-color)
    `(with-safe-alloc (,name
		       (create-image ,width ,height ,true-color)
		       (destroy-image ,name))
       ,@body)))

(defmacro with-image* ((width height &optional true-color) &body body)
  "Creates an image with size WIDTH x HEIGHT and executes BODY with
the image bound to *DEFAULT-IMAGE*. If TRUE-COLOR is true, creates a
true color image. The image is guaranteed to be destroyed before this
macro exits."
  `(with-image (*default-image* ,width ,height ,true-color)
    ,@body))

(defun create-image-from-file (file-name &optional type)
  "Creates an image from the file specified by FILE-NAME \(which is
either a pathname or a string). The type of the image can be provided
as TYPE or otherwise it will be guessed from the PATHNAME-TYPE of
FILE-NAME. You are responsible for destroying the image after you're
done with it. It is advisable to use WITH-IMAGE-FROM-FILE instead."
  (check-type file-name (or pathname string))
  (let* ((pathname-type (pathname-type file-name))
         (%type (or type
                    (cond ((or (string-equal pathname-type "jpg")
                               (string-equal pathname-type "jpeg"))
                            :jpg)
                          ((string-equal pathname-type "png")
                            :png)
                          ((string-equal pathname-type "gd")
                            :gd)
                          ((string-equal pathname-type "gd2")
                            :gd2)
                          ((string-equal pathname-type "xbm")
                            :xbm)
                          #-:win32
                          ((string-equal pathname-type "xpm")
			   :xpm)
                          #-:cl-gd-no-gif
			  ((string-equal pathname-type "gif")
                            :gif)))))
    (unless %type
      (error "No type provided and it couldn't be guessed from filename"))
    (unless (probe-file file-name)
      (error "File ~S could not be found" file-name))
    (when (pathnamep file-name)
      (setq file-name
              #+:cmu (ext:unix-namestring file-name)
              #-:cmu (namestring file-name)))
    (with-foreign-object (err :int)
      (with-cstring (c-file-name file-name)
        (let ((image (ecase %type
                       ((:jpg :jpeg)
                         (gd-image-create-from-jpeg-file c-file-name err))
                       ((:png)
                         (gd-image-create-from-png-file c-file-name err))
                       ((:gd)
                         (gd-image-create-from-gd-file c-file-name err))
                       ((:gd2)
                         (gd-image-create-from-gd2-file c-file-name err))
                       ((:xbm)
                         (gd-image-create-from-xbm-file c-file-name err))
                       #-:win32
                       ((:xpm)
			 (gd-image-create-from-xpm c-file-name))
                       #-:cl-gd-no-gif
		       ((:gif)
                         (gd-image-create-from-gif-file c-file-name err)))))
          (cond ((null-pointer-p image)
                  (cond ((or (eq %type :xpm)
                             (zerop (deref-pointer err :int)))
                          (error "Could not create image from ~A file ~S"
                                 %type file-name))
                        (t
                          (error "Could not create image from ~A file ~S: errno was ~A"
                                 %type file-name (deref-pointer err :int)))))
                (t (let ((image (make-image image)))
		     image))))))))

(defmacro with-image-from-file ((name file-name &optional type) &body body)
  "Creates an image from the file specified by FILE-NAME \(which is
either a pathname or a string) and executes BODY with the image bound
to NAME. The type of the image can be provied as TYPE or otherwise it
will be guessed from the PATHNAME-TYPE of FILE-NAME. The image is
guaranteed to be destroyed before this macro exits."
  ;; we rebind everything so we have left-to-right evaluation
  (with-rebinding (file-name type)
    `(with-safe-alloc (,name
		       (create-image-from-file ,file-name ,type)
		       (destroy-image ,name))
       ,@body)))
      
(defmacro with-image-from-file* ((file-name &optional type) &body body)
  "Creates an image from the file specified by FILE-NAME \(which is
either a pathname or a string) and executes BODY with the image bound
to *DEFAULT-IMAGE*. The type of the image can be provied as TYPE or
otherwise it will be guessed from the PATHNAME-TYPE of FILE-NAME. The
image is guaranteed to be destroyed before this macro exits."
  `(with-image-from-file (*default-image* ,file-name ,type)
    ,@body))

(defun create-image-from-gd2-part (file-name src-x src-y width height)
  "Creates an image from the part of the GD2 file FILE-NAME \(which is
either a pathname or a string) specified by SRC-X, SRC-Y, WIDTH, and
HEIGHT. You are responsible for destroying the image after you're done
with it. It is advisable to use WITH-IMAGE-FROM-GD2-PART instead."
  (check-type file-name (or string pathname))
  (check-type src-x integer)
  (check-type src-y integer)
  (check-type width integer)
  (check-type height integer)
  (unless (probe-file file-name)
    (error "File ~S could not be found" file-name))
  (when (pathnamep file-name)
    (setq file-name
            #+:cmu (ext:unix-namestring file-name)
            #-:cmu (namestring file-name)))
  (with-foreign-object (err :int)
    (with-cstring (c-file-name file-name)
      (let ((image (gd-image-create-from-gd2-part-file c-file-name err src-x src-y width height)))
        (cond ((null-pointer-p image)
                (error "Could not create GD2 image from file ~S: errno was ~A"
                       file-name (deref-pointer err :int)))
              (t image))))))

(defmacro with-image-from-gd2-part ((name file-name src-x src-y width height) &body body)
  "Creates an image from the part of the GD2 file FILE-NAME \(which is
either a pathname or a string) specified SRC-X, SRC-Y, WIDTH, and
HEIGHT and executes BODY with the image bound to NAME. The type of the
image can be provied as TYPE or otherwise it will be guessed from the
PATHNAME-TYPE of FILE-NAME. The image is guaranteed to be destroyed
before this macro exits."
  ;; we rebind everything so we have left-to-right evaluation
  (with-rebinding (file-name src-x src-y width height)
    `(with-safe-alloc (,name
		       (create-image-from-gd2-part ,file-name ,src-x ,src-y ,width ,height)
		       (destroy-image ,name))
       ,@body)))

(defmacro with-image-from-gd2-part* ((file-name src-x src-y width height) &body body)
  "Creates an image from the part of the GD2 file FILE-NAME \(which is
either a pathname or a string) specified SRC-X, SRC-Y, WIDTH, and
HEIGHT and executes BODY with the image bound to *DEFAULT-IMAGE*. The
type of the image can be provied as TYPE or otherwise it will be
guessed from the PATHNAME-TYPE of FILE-NAME. The image is guaranteed
to be destroyed before this macro exits."
  `(with-image-from-gd2-part (*default-image* ,file-name ,src-x ,src-y ,width ,height)
    ,@body))
  
(defmacro make-stream-fn (name signature gd-call type-checks docstring)
  "Internal macro used to generate WRITE-JPEG-TO-STREAM and friends."
  `(defun ,name ,signature
    ,docstring
    ,@type-checks
    (cond ((or #+(and :allegro :allegro-version>= (version>= 6 0))
               (typep stream 'excl:simple-stream)
               #+:lispworks4.3
               (subtypep 'base-char (stream-element-type stream))
               (subtypep '(unsigned-byte 8) (stream-element-type stream)))
            (with-foreign-object (size :int)
	      (with-safe-alloc (memory ,gd-call (gd-free memory))
                (let (#+:lispworks4.3
                      (temp-array (make-array 1 :element-type
                                                '(unsigned-byte 8))))
                  (with-cast-pointer (temp memory :unsigned-byte)
                    (dotimes (i (deref-pointer size :int))
                      ;; LispWorks workaround, WRITE-BYTE won't work - see
                      ;; <http://article.gmane.org/gmane.lisp.lispworks.general/1827>
                      #+:lispworks4.3
                      (setf (aref temp-array 0)
                              (deref-array temp '(:array :unsigned-byte) i))
                      #+:lispworks4.3
                      (write-sequence temp-array stream)
                      #-:lispworks4.3
                      (write-byte (deref-array temp '(:array :unsigned-byte) i)
                                  stream))
                    image)))))
          ((subtypep 'character (stream-element-type stream))
           (with-foreign-object (size :int)
             (with-safe-alloc (memory ,gd-call (gd-free memory))
               (with-cast-pointer (temp memory
                                        #+(or :cmu :scl :sbcl) :unsigned-char
                                        #-(or :cmu :scl :sbcl) :char)
                 (dotimes (i (deref-pointer size :int))
                   (write-char (ensure-char-character
                                (deref-array temp '(:array :char) i))
                               stream))
                 image))))
          (t (error "Can't use a stream with element-type ~A"
                    (stream-element-type stream))))))

(make-stream-fn write-jpeg-to-stream (stream &key (quality -1) (image *default-image*))
                (gd-image-jpeg-ptr (img image) size quality)
                ((check-type stream stream)
                 (check-type quality (integer -1 100))
                 (check-type image image))
                "Writes image IMAGE to stream STREAM as JPEG. If
QUALITY is not specified, the default IJG JPEG quality value is
used. Otherwise, for practical purposes, quality should be a value in
the range 0-95. STREAM must be a character stream or a binary stream
of element type \(UNSIGNED-BYTE 8). If STREAM is a character stream,
the user of this function has to make sure the external format is
yields faithful output of all 8-bit characters.")

(make-stream-fn write-png-to-stream (stream &key compression-level (image *default-image*))
                (cond (compression-level
                        (gd-image-png-ptr-ex (img image) size compression-level))
                      (t
                        (gd-image-png-ptr (img image) size)))
                ((check-type stream stream)
                 (check-type compression-level (or null (integer -1 9)))
                 (check-type image image))
                "Writes image IMAGE to stream STREAM as PNG. If
COMPRESSION-LEVEL is not specified, the default compression level at
the time zlib was compiled on your system will be used.  Otherwise, a
compression level of 0 means 'no compression', a compression level of
1 means 'compressed, but as quickly as possible', a compression level
of 9 means 'compressed as much as possible to produce the smallest
possible file.' STREAM must be a character stream or a binary stream
of element type \(UNSIGNED-BYTE 8). If STREAM is a character stream,
the user of this function has to make sure the external format yields
faithful output of all 8-bit characters.")

#-:cl-gd-no-gif
(make-stream-fn write-gif-to-stream (stream &key (image *default-image*))
		(gd-image-gif-ptr (img image) size)
                ((check-type stream stream)
                 (check-type image image))
                "Writes image IMAGE to stream STREAM as GIF. STREAM
must be a character stream or a binary stream of element type
\(UNSIGNED-BYTE 8). If STREAM is a character stream, the user of this
function has to make sure the external format yields faithful output
of all 8-bit characters.")

(make-stream-fn write-wbmp-to-stream (stream &key foreground (image *default-image*))
                (gd-image-wbmp-ptr (img image) size foreground)
                ((check-type stream stream)
                 (check-type foreground integer)
                 (check-type image image))
                "Writes image IMAGE to stream STREAM as WBMP. STREAM
must be a character stream or a binary stream of element type
\(UNSIGNED-BYTE 8). If STREAM is a character stream, the user of this
function has to make sure the external format yields faithful output
of all 8-bit characters. WBMP file support is black and white
only. The color index specified by the FOREGOUND argument is the
\"foreground,\" and only pixels of this color will be set in the WBMP
file")

(make-stream-fn write-gd-to-stream (stream &key (image *default-image*))
                (gd-image-gd-ptr (img image) size)
                ((check-type stream stream)
                 (check-type image image))
                "Writes image IMAGE to stream STREAM as GD. STREAM
must be a character stream or a binary stream of element type
\(UNSIGNED-BYTE 8). If STREAM is a character stream, the user of this
function has to make sure the external format yields faithful output
of all 8-bit characters.")

(make-stream-fn write-gd2-to-stream (stream &key (image *default-image*))
                (gd-image-gd2-ptr (img image) size)
                ((check-type stream stream)
                 (check-type image image))
                "Writes image IMAGE to stream STREAM as GD2. STREAM
must be a character stream or a binary stream of element type
\(UNSIGNED-BYTE 8). If STREAM is a character stream, the user of this
function has to make sure the external format yields faithful output
of all 8-bit characters.")

(defun write-image-to-stream (stream type &rest rest &key &allow-other-keys)
  "Writes image to STREAM. The type of the image is determined by TYPE
which must be one of :JPG, :JPEG, :PNG, :WBMP, :GD, or :GD2. STREAM
must be a character stream or a binary stream of element type
\(UNSIGNED-BYTE 8). If STREAM is a character stream, the user of this
function has to make sure the external format yields faithful output
of all 8-bit characters."
  (apply (ecase type
           ((:jpg :jpeg)
             #'write-jpeg-to-stream)
           ((:png)
             #'write-png-to-stream)
           ((:wbmp)
             #'write-wbmp-to-stream)
           ((:gd)
             #'write-gd-to-stream)
           ((:gd2)
	     #'write-gd2-to-stream)
           #-:cl-gd-no-gif
	   ((:gif)
             #'write-gif-to-stream))
         stream rest))

(defun write-image-to-file (file-name &rest rest &key type (if-exists :error) &allow-other-keys)
  "Writes image to the file specified by FILE-NAME \(a pathname or a
string). The TYPE argument is interpreted as in
WRITE-IMAGE-TO-STREAM. If it is not provided it is guessed from the
PATHNAME-TYPE of FILE-NAME. The IF-EXISTS keyword argument is given to
OPEN. Other keyword argument like QUALITY or COMPRESSION-LEVEL can be
provided depending on the images's type."
  (with-open-file (stream file-name :direction :output
                                    :if-exists if-exists
                                    :element-type '(unsigned-byte 8))
    (apply #'write-image-to-stream
           stream
           (or type
               (let ((pathname-type (pathname-type (truename file-name))))
                 (cond ((or (string-equal pathname-type "jpg")
                            (string-equal pathname-type "jpeg"))
                         :jpg)
                       ((string-equal pathname-type "png")
                         :png)
                       ((string-equal pathname-type "wbmp")
                         :wbmp)
                       ((string-equal pathname-type "gd")
                         :gd)
                       ((string-equal pathname-type "gd2")
                         :gd2)
                       #-:cl-gd-no-gif
                       ((string-equal pathname-type "gif")
                         :gif)
                       (t
                         (error "Can't determine the type of the image")))))
           (sans rest :type :if-exists))))

(defun image-width (&optional (image *default-image*))
  "Returns width of IMAGE."
  (check-type image image)
  (with-transformed-alternative
      (((gd-image-get-sx (img image)) w-inv-transformer))
    (gd-image-get-sx (img image))))

(defun image-height (&optional (image *default-image*))
  (check-type image image)
  "Returns height of IMAGE."
  (with-transformed-alternative
      (((gd-image-get-sy (img image)) h-inv-transformer))
    (gd-image-get-sy (img image))))

(defun image-size (&optional (image *default-image*))
  (check-type image image)
  "Returns width and height of IMAGE as two values."
  (with-transformed-alternative
      (((gd-image-get-sx (img image)) w-inv-transformer)
       ((gd-image-get-sy (img image)) h-inv-transformer))
    (values (gd-image-get-sx (img image))
            (gd-image-get-sy (img image)))))
