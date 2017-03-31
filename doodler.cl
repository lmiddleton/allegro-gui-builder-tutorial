;;; Code for the form named :form1 of class dialog.
;;; The inspector may add event-handler starter code here,
;;; and you may add other code for this form as well.
;;; The code for recreating the window is auto-generated into 
;;; the *.bil file having the same name as this *.cl file.

(in-package :common-graphics-user)

(defclass doodler (bitmap-window)
  ((doodler-curve-dialog
    :accessor doodler-curve-dialog
    :initform nil)
   (doodler-background-palette
    :initform nil
    :accessor doodler-background-palette)))

(defun doodler-toolbar-click
    (widget new-value old-value)
  (declare
   (ignore-if-unused widget new-value old-value))
  ;; Do the action only when a button is
  ;; being pressed (not unpressed)
  (when new-value
    (let ((doodler (parent (parent widget))))
      (case (first new-value)
        (:erase
         (erase-window doodler))
        (:curve
         (show-curve-dialog doodler))
        (:scroll-to-center
         (scroll-to-center doodler))
        (t nil))))
  (not new-value))

(defmethod show-curve-dialog ((window doodler))
  (let* ((dialog (doodler-curve-dialog window))
         (curve-list nil)) ; step 50
    (when (or (not dialog)
              (not (windowp dialog)))
      (setq dialog
            (make-curve-dialog :owner window))
      (setf (doodler-curve-dialog window) dialog)
      (setq curve-list ; step 50
            (find-component :curve-list dialog))
      (setf (range curve-list) ; step 50
            (list
             (make-instance 'cycloidal-curve)))
      ;; Position the dialog to the
      ;; left of the main window
      (let* ((pos (window-to-screen-units
                      window
                    (make-position
                     (- (+ (exterior-width dialog) 10))
                     40))))
        ;; But don't let it go off the left
        ;; edge of the screen.
        (setf (position-x pos)
          (max 0 (position-x pos)))
        (move-window dialog pos)))
    (select-window dialog)))

(defmethod close :before ((window doodler) &key)
  (let ((curve-dialog (doodler-curve-dialog window))
        (background-palette
         (doodler-background-palette window)))
    (when (and (windowp curve-dialog)
               curve-dialog)
      (close curve-dialog))
    (setf (doodler-curve-dialog window) nil)
    (when (and (windowp background-palette)
               background-palette)
      (close background-palette))
    (setf (doodler-background-palette window) nil)))

(defmethod erase-window ((window doodler))
  (let ((pane (frame-child window)))
    (erase-contents-box pane (page-box pane))))

(defmethod user-close ((window doodler))
  (let ((modal-dialog (modal-window)))
    (if* modal-dialog then
            (beep window)
            (pop-up-message-dialog window "Doodler"
                                   (format nil "~
Close the ~a modal dialog before closing ~
the Doodler."
                                     (title modal-dialog))
                                   warning-icon
                                   "~OK")
       else
            (call-next-method))))

(defmethod initialize-instance :after
  ((window doodler) &rest initargs)
  (declare (ignore initargs))
  (show-background-palette window)
  (select-window window))

(defmethod show-background-palette
    ((window doodler))
  (let ((palette
         (doodler-background-palette window)))
    (unless palette
      (setq palette
            (make-background-palette
             :owner window))
      (select-window palette)
      (setf
       (doodler-background-palette window)
       palette))
    (select-window palette)))