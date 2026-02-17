(in-package :living-server)

(defvar *swank-started* nil
  "Whether the Swank server has been started.")

(defun start-swank ()
  "Start the Swank server for remote eval from the control plane."
  (unless *swank-started*
    (swank:create-server :port *swank-port*
                         :dont-close t
                         :style :spawn)
    (setf *swank-started* t)
    (format t "~&; Swank server started on port ~D~%" *swank-port*)))
