;;; Boot script for the Living Server
;;; Loads the system and starts all services.

(let* ((boot-file (or *load-truename* *load-pathname*))
       (server-dir (make-pathname :directory (pathname-directory boot-file))))
  (push server-dir asdf:*central-registry*))

(ql:quickload "living-server" :silent t)
(living-server:start-all)

;; Keep the process alive (Swank + Woo run in background threads)
(loop (sleep 60))
