(in-package :cl-gd)

(defun test-anim-1 ()
  (with-image (im 100 100)
    (let* ((black (allocate-color 0 0 0 :image im))
           (blue (allocate-color 255 0 255 :image im))
           (anim (gd-image-gif-anim-begin-wrap (img im)
                                              "test-anim-2.gif"
                                              1
                                              10)))
      (declare (ignore black))
      (unwind-protect
           (loop with last
                 for i from 0 upto 100 by 2
                 for cim = (create-image 100 100)
                 do (copy-palette im cim)
                    (gd-image-arc (img cim) 50 50 i i 0 360 blue)
                    (gd-image-gif-anim-add (img cim) anim 0 0 0 10 +gd-disposal-none+ last)
                    (when last
                      (gd-image-destroy last))
                    (setf last (img cim))
                 finally (gd-image-destroy last))
        (gd-image-gif-anim-end-wrap anim)))))

(defun test-anim-2 ()
  (with-image* (100 100)
    (setf (transparent-color) (allocate-color 0 0 0))
    (let ((blue (allocate-color 0 0 255)))
      (with-animated-gif ("test-anim.gif"
                          :global-color-map-p t
                          :loop-count -1
                          :default-delay 5)
        (loop with last-image
              for i from 0 upto 100 by 10
              for cim = (create-image 100 100)
              do (draw-arc 50 50 i i 0 360 :filled t :color blue :image cim)
                 (add-image-to-animation cim :last-image last-image)
                 (when last-image
                   (destroy-image last-image))
                 (setf last-image cim)
              finally (destroy-image last-image))))))

(with-image* (100 100)
  (fill-image 0 0 :color (allocate-color 110 110 110))
  (setf (transparent-color) (allocate-color 0 0 0))
  (let ((blue (allocate-color 0 0 255)))
    (with-animated-gif ("test-anim.gif"
                        :global-color-map-p t
                        :loop-count -1
                        :default-delay 5)
      (loop for i from 0 upto 100 by 10
            for cim = (create-image 100 100)
            do (draw-arc i i 20 20 0 360 :filled t :color blue :image cim)
               (add-image-to-animation cim :disposal :restore-background)))))

(with-image-from-file* ("test/triangle.png")
  (let ((blue (allocate-color 0 0 255)))
    (with-animated-gif ("test-anim.gif"
                        :global-color-map-p t
                        :loop-count -1
                        :default-delay 5)
      (loop for i from 0 upto 100 by 10
            for cim = (create-image 100 100)
            do (draw-arc i i 20 20 0 360 :filled t :color blue :image cim)
               (add-image-to-animation cim :disposal :restore-previous)))))
