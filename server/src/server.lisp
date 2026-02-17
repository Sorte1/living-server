(in-package :living-server)

(defvar *server* nil
  "The running Woo server instance.")

(defun load-generated-code ()
  "Load all generated code files from the manifest."
  (let ((manifest (merge-pathnames "generated/manifest.lisp"
                                   (asdf:system-source-directory "living-server"))))
    (when (probe-file manifest)
      (format t "~&; Loading generated code from manifest...~%")
      (load manifest))))

(defun start-server ()
  "Start the Woo HTTP server."
  (when *server*
    (format t "~&; Server already running, stopping first...~%")
    (stop-server))
  (load-generated-code)
  (format t "~&; Starting HTTP server on port ~D...~%" *http-port*)
  (setf *server*
        (bt:make-thread
         (lambda ()
           (woo:run (lack.builder:builder *app*)
                    :port *http-port*
                    :address "0.0.0.0"))
         :name "woo-server"))
  (format t "~&; HTTP server started on port ~D~%" *http-port*))

(defun stop-server ()
  "Stop the Woo HTTP server."
  (when *server*
    (bt:destroy-thread *server*)
    (setf *server* nil)
    (format t "~&; HTTP server stopped~%")))

(defun server-status ()
  "Return the server status as a plist."
  (list :running (and *server* (bt:thread-alive-p *server*))
        :http-port *http-port*
        :swank-port *swank-port*))

(defun start-all ()
  "Start everything: Swank + HTTP server."
  (format t "~&; === Living Server starting ===~%")
  (start-swank)
  (start-server)
  (format t "~&; === Living Server ready ===~%")
  (format t "~&; HTTP:  http://localhost:~D~%" *http-port*)
  (format t "~&; Swank: localhost:~D~%" *swank-port*))

(defun stop-all ()
  "Stop everything."
  (stop-server)
  (format t "~&; === Living Server stopped ===~%"))
