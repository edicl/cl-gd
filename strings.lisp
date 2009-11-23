;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/strings.lisp,v 1.24 2009/11/23 17:05:39 edi Exp $

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

(defmacro with-default-font ((font) &body body)
  "Execute BODY with *DEFAULT-FONT* bound to FONT so that you
don't have to provide the FONT keyword/optional argument to
string functions. But note that the fonts used for
DRAW-STRING/DRAW-CHARACTER and DRAW-FREETYPE-STRING are
incompatible."
  `(let ((*default-font* ,font))
    ,@body))

(defun draw-character (x y char &key up (font *default-font*) (color *default-color*) (image *default-image*))
  "Draws the character CHAR from font FONT in color COLOR at position
\(X,Y). If UP is true the character will be drawn from bottom to top
\(rotated 90 degrees). FONT must be one of :TINY, :SMALL, :MEDIUM,
:LARGE, :GIANT."
  (check-type char character)
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((x x-transformer)
         (y y-transformer))
      (if up
        (gd-image-char-up (img image) (ecase font
                                        ((:tiny) +gd-font-tiny+)
                                        ((:small) +gd-font-small+)
                                        ((:medium :medium-bold) +gd-font-medium-bold+)
                                        ((:large) +gd-font-large+)
                                        ((:giant) +gd-font-giant+))
                          x y (char-code char) color)
        (gd-image-char (img image) (ecase font
                                     ((:tiny) +gd-font-tiny+)
                                     ((:small) +gd-font-small+)
                                     ((:medium :medium-bold) +gd-font-medium-bold+)
                                     ((:large) +gd-font-large+)
                                     ((:giant) +gd-font-giant+))
                       x y (char-code char) color))))
  char)

(defun draw-string (x y string &key up (font *default-font*) (color *default-color*) (image *default-image*))
  "Draws the string STRING in color COLOR at position \(X,Y). If UP is
true the character will be drawn from bottom to top \(rotated 90
degrees). FONT must be one of :TINY, :SMALL, :MEDIUM, :LARGE, :GIANT."
  (check-type string string)
  (check-type image image)
  (with-color-argument
    (with-transformed-alternative
        ((x x-transformer)
         (y y-transformer))
      (with-cstring (c-string string)
        (if up
          (gd-image-string-up (img image) (ecase font
                                            ((:tiny) +gd-font-tiny+)
                                            ((:small) +gd-font-small+)
                                            ((:medium :medium-bold) +gd-font-medium-bold+)
                                            ((:large) +gd-font-large+)
                                            ((:giant) +gd-font-giant+))
                              x y c-string color)
          (gd-image-string (img image) (ecase font
                                         ((:tiny) +gd-font-tiny+)
                                         ((:small) +gd-font-small+)
                                         ((:medium :medium-bold) +gd-font-medium-bold+)
                                         ((:large) +gd-font-large+)
                                         ((:giant) +gd-font-giant+))
                           x y c-string color)))))
  string)

(defun draw-freetype-string (x y string
                             &key (anti-aliased t)
                                  (point-size 12.0d0)
                                  (angle 0.0d0)
                                  (convert-chars t)
                                  line-spacing
                                  (font-name *default-font*)
                                  do-not-draw
                                  (color *default-color*)
                                  (image *default-image*))
  "Draws the string STRING in color COLOR at position \(X,Y) using the
FreeType library. FONT-NAME is the full path \(a pathname or a string)
to a TrueType font file, or a font face name if the GDFONTPATH
environment variable or FreeType's DEFAULT_FONTPATH variable have been
set intelligently. The string may be arbitrarily scaled \(POINT-SIZE)
and rotated \(ANGLE in radians). The direction of rotation is
counter-clockwise, with 0 radians \(0 degrees) at 3 o'clock and PI/2
radians \(90 degrees) at 12 o'clock. Note that the ANGLE argument is
purposefully _not_ affected by WITH-TRANSFORMATION. If ANTI-ALIASED if
false, anti-aliasing is disabled. It is enabled by default. To output
multiline text with a specific line spacing, provide a value for
LINE-SPACING, expressed as a multiple of the font height. The default
is to use 1.05. The string may contain XML character entity references
like \"&#192;\". If CONVERT-CHARS is true \(which is the default)
characters of STRING with CHAR-CODE greater than 127 are converted
accordingly. This of course pre-supposes that your Lisp's CHAR-CODE
function returns ISO/IEC 10646 (Unicode) character codes.

The return value is an array containing 8 elements representing the 4
corner coordinates \(lower left, lower right, upper right, upper left)
of the bounding rectangle around the string that was drawn. The points
are relative to the text regardless of the angle, so \"upper left\"
means in the top left-hand corner seeing the text horizontally. Set
DO-NOT-DRAW to true to get the bounding rectangle without
rendering. This is a relatively cheap operation if followed by a
rendering of the same string, because of the caching of the partial
rendering during bounding rectangle calculation."
  (check-type string string)
  (check-type font-name (or pathname string))
  (unless do-not-draw
    (check-type color integer)
    (check-type image image))
  (with-transformed-alternative
      ((x x-transformer)
       (y y-transformer)
       ((deref-array c-bounding-rectangle '(:array :int) i) x-inv-transformer)
       ((deref-array c-bounding-rectangle '(:array :int) (1+ i)) y-inv-transformer))
    (when do-not-draw
      (setq color 0
            image *null-image*))
    (when (pathnamep font-name)
      (setq font-name (namestring font-name)))
    (when convert-chars
      (setq string (convert-to-char-references string)))
    (with-cstring (c-font-name font-name)
      (with-cstring (c-string string)
        (with-safe-alloc (c-bounding-rectangle
                          (allocate-foreign-object :int 8)
                          (free-foreign-object c-bounding-rectangle))
          (let ((msg (convert-from-cstring
                      (cond (line-spacing
                             (with-foreign-object (strex 'gd-ft-string-extra)
                               (setf (get-slot-value strex
                                                     'gd-ft-string-extra
                                                     'flags)
                                     +gd-ftex-linespace+
                                     (get-slot-value strex
                                                     'gd-ft-string-extra
                                                     'line-spacing)
                                         (coerce line-spacing 'double-float))
                               (gd-image-string-ft-ex (img image)
                                                      c-bounding-rectangle
                                                      (if anti-aliased color (- color))
                                                      c-font-name
                                                      (coerce point-size 'double-float)
                                                      (coerce angle 'double-float)
                                                      x y
                                                      c-string
                                                      strex)))
                            (t
                             (gd-image-string-ft (img image)
                                                 c-bounding-rectangle
                                                 (if anti-aliased color (- color))
                                                 c-font-name
                                                 (coerce point-size 'double-float)
                                                 (coerce angle 'double-float)
                                                 x y
                                                 c-string))))))
            (when msg
              (error "Error in FreeType library: ~A" msg))
            (let ((bounding-rectangle (make-array 8)))
              ;; strange iteration due to WITH-TRANSFORMED-ALTERNATIVE
              (loop for i below 8 by 2 do
                    (setf (aref bounding-rectangle i)
                          (deref-array c-bounding-rectangle '(:array :int) i))
                    (setf (aref bounding-rectangle (1+ i))
                          (deref-array c-bounding-rectangle '(:array :int) (1+ i))))
              bounding-rectangle)))))))