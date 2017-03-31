;;; Code for the form named :form145 of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)

(defclass background-palette (color-mixin dialog)
  ())

(defun background-palette-color-list-on-change
    (widget new-value old-value)
  (declare
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (change-background-color (parent widget)))
  (not new-value))

(defmethod change-background-color
    ((palette background-palette))
  (let ((color (current-color palette))
        (doodler (owner palette)))
    (setf
     (background-color (frame-child doodler))
     color)
    (erase-window doodler)))

(defun background-palette-color-button-on-change
    (widget new-value old-value)
  (declare
   (ignore-if-unused widget new-value old-value))
  (when new-value
    (add-other-color (parent widget)))
  (not new-value))

(defmethod initialize-instance :after
  ((palette background-palette) &rest initargs)
  (declare (ignore initargs))
  (let* ((pane (frame-child (owner palette)))
         (color-name (find-color-name palette
                                      (or (background-color pane)
                                          (default-background-color pane)))))
    (initialize-value
     (find-component :color-list palette)
     (list color-name))))