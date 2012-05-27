;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-GD; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/gd/gd-uffi.lisp,v 1.33 2009/11/23 17:05:39 edi Exp $

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

;; internal representation of an image in GD
(def-struct gd-image
  (pixels (* (* :unsigned-char)))
  (sx :int)
  (sy :int)
  (colors-total :int)
  (red (:array :int #.+max-colors+))
  (green (:array :int #.+max-colors+))
  (blue (:array :int #.+max-colors+))
  (open (:array :int #.+max-colors+))
  (transparent :int)
  (poly-ints (* :int))
  (poly-allocated :int)
  (brush :pointer-self)
  (tile :pointer-self)
  (brush-color-map (:array :int #.+max-colors+))
  (tile-color-map (:array :int #.+max-colors+))
  (style-length :int)
  (style-pos :int)
  (style (* :int))
  (interface :int)
  (thick :int)
  (alpha (:array :int #.+max-colors+))
  (true-color :int)
  (t-pixels (* (* :int)))
  (alpha-blending-flag :int)
  (save-alpha-flag :int)
  (aa :int)
  (aa-color :int)
  (aa-do-not-blend :int)
  (aa-opacity (* (* :unsigned-char)))
  (aa-polygon :int)
  (aal-x1 :int)
  (aal-y1 :int)
  (aal-x2 :int)
  (aal-y2 :int)
  (aal-bx-ax :int)
  (aal-by-ay :int)
  (aal-lab-2 :int)
  (aal-lab :float)
  (cx1 :int)
  (cy1 :int)
  (cx2 :int)
  (cy2 :int))

(def-type pixels-array (* (* :unsigned-char)))
(def-type pixels-row (* :unsigned-char))
(def-type t-pixels-array (* (* :int)))
(def-type t-pixels-row (* :int))

(def-foreign-type gd-image-ptr (* gd-image))

;; initialize special variable
(setq *null-image* (make-image (make-null-pointer 'gd-image)))

;; internal representation of a point in GD, used by the polygon
;; functions
(def-struct gd-point
  (x :int)
  (y :int))

(def-foreign-type gd-point-ptr (* gd-point))

;; internal representation of a font in GD, used by the (non-FreeType)
;; functions which draw characters and strings
(def-struct gd-font
  (nchars :int)
  (offset :int)
  (w :int)
  (h :int)
  (data (* :char)))

(def-foreign-type gd-font-ptr (* gd-font))

;; additional info for calls to the FreeType library - currently only
;; used for line spacing
(def-struct gd-ft-string-extra
  (flags :int)
  (line-spacing :double)
  (charmap :int))

(def-foreign-type gd-ft-string-extra-ptr (* gd-ft-string-extra))

;; the GD standard fonts used when drawing characters or strings
;; without invoking the FreeType library
(def-foreign-var ("gdFontTiny" +gd-font-tiny+) gd-font-ptr "gd")
(def-foreign-var ("gdFontSmall" +gd-font-small+) gd-font-ptr "gd")
(def-foreign-var ("gdFontMediumBold" +gd-font-medium-bold+) gd-font-ptr "gd")
(def-foreign-var ("gdFontLarge" +gd-font-large+) gd-font-ptr "gd")
(def-foreign-var ("gdFontGiant" +gd-font-giant+) gd-font-ptr "gd")

;;; all GD functions which are accessed from CL-GD

(def-function ("gdImageCreate" gd-image-create)
    ((sx :int)
     (sy :int))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageCreateTrueColor" gd-image-create-true-color)
    ((sx :int)
     (sy :int))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageCreateFromJpegFile" gd-image-create-from-jpeg-file)
    ((filename :cstring)
     (err (* :int)))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageCreateFromPngFile" gd-image-create-from-png-file)
    ((filename :cstring)
     (err (* :int)))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageCreateFromGdFile" gd-image-create-from-gd-file)
    ((filename :cstring)
     (err (* :int)))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageCreateFromGd2File" gd-image-create-from-gd2-file)
    ((filename :cstring)
     (err (* :int)))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageCreateFromGd2PartFile" gd-image-create-from-gd2-part-file)
    ((filename :cstring)
     (err (* :int))
     (src-x :int)
     (src-y :int)
     (w :int)
     (h :int))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageCreateFromXbmFile" gd-image-create-from-xbm-file)
    ((filename :cstring)
     (err (* :int)))
  :returning gd-image-ptr
  :module "gd")

#-:win32
(def-function ("gdImageCreateFromXpm" gd-image-create-from-xpm)
    ((filename :cstring))
  :returning gd-image-ptr
  :module "gd")

#-:cl-gd-no-gif
(def-function ("gdImageCreateFromGifFile" gd-image-create-from-gif-file)
    ((filename :cstring)
     (err (* :int)))
  :returning gd-image-ptr
  :module "gd")

(def-function ("gdImageJpegPtr" gd-image-jpeg-ptr)
    ((im gd-image-ptr)
     (size (* :int))
     (quality :int))
  :returning :pointer-void
  :module "gd")

(def-function ("gdImageGdPtr" gd-image-gd-ptr)
    ((im gd-image-ptr)
     (size (* :int)))
  :returning :pointer-void
  :module "gd")

(def-function ("gdImageGd2Ptr" gd-image-gd2-ptr)
    ((im gd-image-ptr)
     (size (* :int)))
  :returning :pointer-void
  :module "gd")

(def-function ("gdImageWBMPPtr" gd-image-wbmp-ptr)
    ((im gd-image-ptr)
     (size (* :int))
     (fg :int))
  :returning :pointer-void
  :module "gd")

(def-function ("gdImagePngPtr" gd-image-png-ptr)
    ((im gd-image-ptr)
     (size (* :int)))
  :returning :pointer-void
  :module "gd")

(def-function ("gdImagePngPtrEx" gd-image-png-ptr-ex)
    ((im gd-image-ptr)
     (size (* :int))
     (level :int))
  :returning :pointer-void
  :module "gd")

#-:cl-gd-no-gif
(def-function ("gdImageGifPtr" gd-image-gif-ptr)
    ((im gd-image-ptr)
     (size (* :int)))
  :returning :pointer-void
  :module "gd")

(def-function ("gdImageDestroy" gd-image-destroy)
    ((im gd-image-ptr))
  :returning :void
  :module "gd")

(def-function ("gdImageColorAllocate" gd-image-color-allocate)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorAllocateAlpha" gd-image-color-allocate-alpha)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int)
     (a :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorDeallocate" gd-image-color-deallocate)
    ((im gd-image-ptr)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageColorExact" gd-image-color-exact)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorClosest" gd-image-color-closest)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorClosestHWB" gd-image-color-closest-hwb)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorClosestAlpha" gd-image-color-closest-alpha)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int)
     (a :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorResolve" gd-image-color-resolve)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorResolveAlpha" gd-image-color-resolve-alpha)
    ((im gd-image-ptr)
     (r :int)
     (g :int)
     (b :int)
     (a :int))
  :returning :int
  :module "gd")

(def-function ("gdImageColorTransparent" gd-image-color-transparent)
    ((im gd-image-ptr)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageGetGetTransparent" gd-image-get-transparent)
    ((im gd-image-ptr))
  :returning :int
  :module "gd")

(def-function ("gdImageSetAntiAliased" gd-image-set-anti-aliased)
    ((im gd-image-ptr)
     (c :int))
  :returning :void
  :module "gd")

(def-function ("gdImageSetAntiAliasedDontBlend" gd-image-set-anti-aliased-do-not-blend)
    ((im gd-image-ptr)
     (c :int)
     (dont-blend :int))
  :returning :void
  :module "gd")

(def-function ("gdImageSetBrush" gd-image-set-brush)
    ((im gd-image-ptr)
     (brush gd-image-ptr))
  :returning :void
  :module "gd")

(def-function ("gdImageSetTile" gd-image-set-tile)
    ((im gd-image-ptr)
     (tile gd-image-ptr))
  :returning :void
  :module "gd")

(def-function ("gdImageSetStyle" gd-image-set-style)
    ((im gd-image-ptr)
     (style (* :int))
     (style-length :int))
  :returning :void
  :module "gd")

(def-function ("gdImageSetThickness" gd-image-set-thickness)
    ((im gd-image-ptr)
     (thickness :int))
  :returning :void
  :module "gd")

(def-function ("gdImageAlphaBlending" gd-image-alpha-blending)
    ((im gd-image-ptr)
     (blending :int))
  :returning :void
  :module "gd")

(def-function ("gdImageSaveAlpha" gd-image-save-alpha)
    ((im gd-image-ptr)
     (save-flag :int))
  :returning :void
  :module "gd")

(def-function ("gdImageGetRed" gd-image-get-red)
    ((im gd-image-ptr)
     (color :int))
  :returning :int
  :module "gd")

(def-function ("gdImageGetGreen" gd-image-get-green)
    ((im gd-image-ptr)
     (color :int))
  :returning :int
  :module "gd")

(def-function ("gdImageGetBlue" gd-image-get-blue)
    ((im gd-image-ptr)
     (color :int))
  :returning :int
  :module "gd")

(def-function ("gdImageGetAlpha" gd-image-get-alpha)
    ((im gd-image-ptr)
     (color :int))
  :returning :int
  :module "gd")

(def-function ("gdImageGetColorsTotal" gd-image-get-colors-total)
    ((im gd-image-ptr))
  :returning :int
  :module "gd")

(def-function ("gdImageSetClip" gd-image-set-clip)
    ((im gd-image-ptr)
     (x1 :int)
     (y1 :int)
     (x2 :int)
     (y2 :int))
  :returning :void
  :module "gd")

(def-function ("gdImageGetClip" gd-image-get-clip)
    ((im gd-image-ptr)
     (x1p (* :int))
     (y1p (* :int))
     (x2p (* :int))
     (y2p (* :int)))
  :returning :void
  :module "gd")

(def-function ("gdImageSetPixel" gd-image-set-pixel)
    ((im gd-image-ptr)
     (x :int)
     (y :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageLine" gd-image-line)
    ((im gd-image-ptr)
     (x1 :int)
     (y1 :int)
     (x2 :int)
     (y2 :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImagePolygon" gd-image-polygon)
    ((im gd-image-ptr)
     (points gd-point-ptr)
     (points-total :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageFilledPolygon" gd-image-filled-polygon)
    ((im gd-image-ptr)
     (points gd-point-ptr)
     (points-total :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageRectangle" gd-image-rectangle)
    ((im gd-image-ptr)
     (x1 :int)
     (y1 :int)
     (x2 :int)
     (y2 :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageFilledRectangle" gd-image-filled-rectangle)
    ((im gd-image-ptr)
     (x1 :int)
     (y1 :int)
     (x2 :int)
     (y2 :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageFilledEllipse" gd-image-filled-ellipse)
    ((im gd-image-ptr)
     (cx :int)
     (cy :int)
     (w :int)
     (h :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageArc" gd-image-arc)
    ((im gd-image-ptr)
     (cx :int)
     (cy :int)
     (w :int)
     (h :int)
     (s :int)
     (e :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageFilledArc" gd-image-filled-arc)
    ((im gd-image-ptr)
     (cx :int)
     (cy :int)
     (w :int)
     (h :int)
     (s :int)
     (e :int)
     (color :int)
     (style :int))
  :returning :void
  :module "gd")

(def-function ("gdImageFill" gd-image-fill)
    ((im gd-image-ptr)
     (x :int)
     (y :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageFillToBorder" gd-image-fill-to-border)
    ((im gd-image-ptr)
     (x :int)
     (y :int)
     (border :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageChar" gd-image-char)
    ((im gd-image-ptr)
     (f gd-font-ptr)
     (x :int)
     (y :int)
     (c :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageCharUp" gd-image-char-up)
    ((im gd-image-ptr)
     (f gd-font-ptr)
     (x :int)
     (y :int)
     (c :int)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageString" gd-image-string)
    ((im gd-image-ptr)
     (f gd-font-ptr)
     (x :int)
     (y :int)
     (s :cstring)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageStringUp" gd-image-string-up)
    ((im gd-image-ptr)
     (f gd-font-ptr)
     (x :int)
     (y :int)
     (s :cstring)
     (color :int))
  :returning :void
  :module "gd")

(def-function ("gdImageStringFT" gd-image-string-ft)
    ((im gd-image-ptr)
     (brect (* :int))
     (fg :int)
     (fontname :cstring)
     (ptsize :double)
     (angle :double)
     (x :int)
     (y :int)
     (string :cstring))
  :returning :cstring
  :module "gd")

(def-function ("gdImageStringFTEx" gd-image-string-ft-ex)
    ((im gd-image-ptr)
     (brect (* :int))
     (fg :int)
     (fontname :cstring)
     (ptsize :double)
     (angle :double)
     (x :int)
     (y :int)
     (string :cstring)
     (strex gd-ft-string-extra-ptr))
  :returning :cstring
  :module "gd")

(def-function ("gdImageGetPixel" gd-image-get-pixel)
    ((im gd-image-ptr)
     (x :int)
     (y :int))
  :returning :int
  :module "gd")

(def-function ("gdImageBoundsSafe" gd-image-bounds-safe)
    ((im gd-image-ptr)
     (x :int)
     (y :int))
  :returning :int
  :module "gd")

(def-function ("gdImageGetSX" gd-image-get-sx)
    ((im gd-image-ptr))
  :returning :int
  :module "gd")

(def-function ("gdImageGetSY" gd-image-get-sy)
    ((im gd-image-ptr))
  :returning :int
  :module "gd")

(def-function ("gdImageInterlace" gd-image-interlace)
    ((im gd-image-ptr)
     (interlace :int))
  :returning :void
  :module "gd")

(def-function ("gdImageGetGetInterlaced" gd-image-get-interlaced)
    ((im gd-image-ptr))
  :returning :int
  :module "gd")

(def-function ("gdImageCopy" gd-image-copy)
    ((dst gd-image-ptr)
     (src gd-image-ptr)
     (dest-x :int)
     (dest-y :int)
     (src-x :int)
     (src-y :int)
     (w :int)
     (h :int))
  :returning :void
  :module "gd")

(def-function ("gdImageCopyMerge" gd-image-copy-merge)
    ((dst gd-image-ptr)
     (src gd-image-ptr)
     (dest-x :int)
     (dest-y :int)
     (src-x :int)
     (src-y :int)
     (w :int)
     (h :int)
     (percent :int))
  :returning :void
  :module "gd")

(def-function ("gdImageCopyMergeGray" gd-image-copy-merge-gray)
    ((dst gd-image-ptr)
     (src gd-image-ptr)
     (dest-x :int)
     (dest-y :int)
     (src-x :int)
     (src-y :int)
     (w :int)
     (h :int)
     (percent :int))
  :returning :void
  :module "gd")

(def-function ("gdImageCopyResized" gd-image-copy-resized)
    ((dst gd-image-ptr)
     (src gd-image-ptr)
     (dst-x :int)
     (dst-y :int)
     (src-x :int)
     (src-y :int)
     (dest-w :int)
     (dest-h :int)
     (src-w :int)
     (src-h :int))
  :returning :void
  :module "gd")

(def-function ("gdImageCopyResampled" gd-image-copy-resampled)
    ((dst gd-image-ptr)
     (src gd-image-ptr)
     (dst-x :int)
     (dst-y :int)
     (src-x :int)
     (src-y :int)
     (dest-w :int)
     (dest-h :int)
     (src-w :int)
     (src-h :int))
  :returning :void
  :module "gd")

(def-function ("gdImageCopyRotated" gd-image-copy-rotated)
    ((dst gd-image-ptr)
     (src gd-image-ptr)
     (dst-x :double)
     (dst-y :double)
     (src-x :int)
     (src-y :int)
     (src-w :int)
     (src-h :int)
     (angle :int))
  :returning :void
  :module "gd")

(def-function ("gdImagePaletteCopy" gd-image-palette-copy)
    ((dst gd-image-ptr)
     (src gd-image-ptr))
  :returning :void
  :module "gd")

(def-function ("gdImageCompare" gd-image-compare)
    ((im1 gd-image-ptr)
     (im2 gd-image-ptr))
  :returning :int
  :module "gd")

(def-function ("gdImageTrueColorToPalette" gd-image-true-color-to-palette)
    ((im gd-image-ptr)
     (dither :int)
     (colors-wanted :int))
  :returning :void
  :module "gd")

(def-function ("gdFree" gd-free)
    ((ptr :pointer-void))
  :returning :void
  :module "gd")

(def-function ("gdImageGifAnimBeginWrap" gd-image-gif-anim-begin-wrap)
    ((im gd-image-ptr)
     (filename :cstring)
     (global-cm :int)
     (loops :int))
  :returning :pointer-void
  :module "gd")

(def-function ("gdImageGifAnimAdd" gd-image-gif-anim-add)
    ((im gd-image-ptr)
     (out :pointer-void)
     (local-cm :int)
     (left-ofs :int)
     (top-ofs :int)
     (delay :int)
     (disposal :int)
     (previm gd-image-ptr))
  :returning :void
  :module "gd")

(def-function ("gdImageGifAnimEndWrap" gd-image-gif-anim-end-wrap)
    ((out :pointer-void))
  :returning :void
  :module "gd")