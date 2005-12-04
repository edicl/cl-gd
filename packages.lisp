(in-package #:cl-user)

(defpackage #:cl-gd
  (:use #:cl #:uffi)
  (:export #:*default-image*
           #:*default-color*
           #:*default-font*
           #:+max-colors+
           #:without-transformations
           #:with-transformation
           #:create-image
           #:destroy-image
           #:with-image
           #:create-image-from-file
           #:with-image-from-file
           #:create-image-from-gd2-part
           #:with-image-from-gd2-part
           #:with-default-image
           #:with-image*
           #:with-image-from-file*
           #:with-image-from-gd2-part*
           #:write-jpeg-to-stream
           #:write-png-to-stream
           #:write-wbmp-to-stream
           #:write-gd-to-stream
           #:write-gd2-to-stream
           #-:cl-gd-no-gif #:write-gif-to-stream
           #:write-image-to-stream
           #:write-image-to-file
           #:image-width
           #:image-height
           #:image-size
           #:make-brush
           #:make-tile
           #:make-anti-aliased
           #:with-default-color
           #:allocate-color
           #:deallocate-color
           #:transparent-color
           #:true-color-p
           #:number-of-colors
           #:find-color
           #:find-color-from-image
           #:thickness
           #:with-thickness
           #:alpha-blending-p
           #:save-alpha-p
           #:color-component
           #:color-components      
           #:draw-polygon
           #:draw-line
           #:get-pixel
           #:set-pixel
           #:set-pixels
           #:draw-rectangle
           #:draw-rectangle*
           #:draw-arc
           #:draw-filled-ellipse
           #:draw-filled-circle
           #:fill-image
           #:clipping-rectangle
           #:clipping-rectangle*
           #:set-clipping-rectangle*
           #:with-clipping-rectangle
           #:with-clipping-rectangle*
           #:with-default-font
           #:draw-character
           #:draw-string
           #:draw-freetype-string
           #:interlacedp
           #:differentp
           #:copy-image
           #:copy-palette
           #:true-color-to-palette
           #:do-rows
           #:do-pixels-in-row
           #:do-pixels
           #:raw-pixel))

(pushnew :cl-gd *features*)
