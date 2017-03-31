;;; Code for the form named :form81 of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)

(defclass coefficient-dialog (color-mixin dialog)
  ())

(defun coefficient-dialog-test-button-on-change
    (widget new-value old-value)
  (declare
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (test-curve (parent widget)))
  (not new-value))

(defmethod test-curve ((dialog coefficient-dialog))
  (let* ((curve
          (make-instance 'cycloidal-curve
            :a-coefficient
            (value (find-component
                    :a-coefficient-control
                    dialog))
            :b-coefficient
            (value (find-component
                    :b-coefficient-control
                    dialog))
            :c-coefficient
            (value (find-component
                    :c-coefficient-control
                    dialog))
            :color (current-color dialog)))
         (curve-dialog (owner dialog))
         (doodler (owner curve-dialog)))
    (draw-curve (frame-child doodler) curve)))

(defun coefficient-dialog-color-button-on-change
    (widget new-value old-value)
  (declare
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (add-other-color (parent widget)))
  (not new-value))