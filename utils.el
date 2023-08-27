(require 'cl-lib)

(window-absolute-pixel-position (point) nil)

(point)

(posn-point (posn-at-x-y 395 64))

(defun get-active-monitor-resolution()
  "Get the resolution of the active monitor.
   Active means the monitor that emacs is running on.
   Return value is a list of the form (width height)."
  (interactive)
  (let ((monitors (display-monitor-attributes-list)))
    (let ((active-monitor (car
                           (cl-remove-if
                            (lambda (x) (null (cdr (assoc 'frames x))))
                            monitors))))
      (let ((geometry (assoc 'geometry active-monitor)))
        (let ((resolution (list (nth 3 geometry) (nth 4 geometry))))
          resolution)))))

(get-active-monitor-resolution)
