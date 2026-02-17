(in-package :living-server)

(defparameter *http-port* 3001
  "Port for the user-facing HTTP server.")

(defparameter *swank-port* 4005
  "Port for the Swank server (control plane connects here).")
