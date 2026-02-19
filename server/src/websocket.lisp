(in-package :living-server)

;;; WebSocket support for the living server.
;;; WebSocket routes are registered separately from ningle HTTP routes
;;; because ningle doesn't handle WebSocket upgrades. The server.lisp
;;; wrapper checks incoming requests against these routes first.

(defvar *websocket-routes* (make-hash-table :test 'equal)
  "Map of path -> handler-spec for WebSocket routes.
Each handler-spec is a plist (:on-open fn :on-message fn :on-close fn).")

(defvar *websocket-connections* (make-hash-table :test 'equal)
  "Map of path -> list of active WebSocket connections.")

(defvar *ws-lock* (bt:make-lock "websocket-connections-lock"))

(defun add-websocket-route (path &key on-open on-message on-close)
  "Register a WebSocket route. Handlers:
  :on-open (lambda (ws)) - called when a client connects
  :on-message (lambda (ws message)) - called when a message is received
  :on-close (lambda (ws)) - called when a client disconnects"
  (setf (gethash path *websocket-routes*)
        (list :on-open on-open
              :on-message on-message
              :on-close on-close))
  (bt:with-lock-held (*ws-lock*)
    (unless (gethash path *websocket-connections*)
      (setf (gethash path *websocket-connections*) '())))
  (format t "~&; WebSocket route registered: ~A~%" path))

(defun remove-websocket-route (path)
  "Remove a WebSocket route."
  (remhash path *websocket-routes*)
  (bt:with-lock-held (*ws-lock*)
    (remhash path *websocket-connections*)))

(defun websocket-broadcast (path message)
  "Send a message to all connected WebSocket clients on a path."
  (let ((connections (bt:with-lock-held (*ws-lock*)
                       (copy-list (gethash path *websocket-connections*)))))
    (dolist (ws connections)
      (handler-case
          (wsd:send ws message)
        (error () nil)))))

(defun websocket-send (ws message)
  "Send a message to a single WebSocket connection."
  (wsd:send ws message))

(defun handle-websocket-request (env path)
  "Handle a WebSocket upgrade request for the given path.
Returns a Clack response for Woo's WebSocket handling."
  (let* ((spec (gethash path *websocket-routes*))
         (on-open (getf spec :on-open))
         (on-message (getf spec :on-message))
         (on-close (getf spec :on-close))
         (ws (wsd:make-server env)))
    ;; :open callback receives no extra args
    (wsd:on :open ws
      (lambda ()
        (bt:with-lock-held (*ws-lock*)
          (push ws (gethash path *websocket-connections*)))
        (when on-open
          (funcall on-open ws))))
    ;; :message callback receives the raw message string directly
    (wsd:on :message ws
      (lambda (message)
        (when on-message
          (funcall on-message ws message))))
    ;; :close callback receives &key code reason
    (wsd:on :close ws
      (lambda (&key code reason)
        (declare (ignore code reason))
        (bt:with-lock-held (*ws-lock*)
          (setf (gethash path *websocket-connections*)
                (remove ws (gethash path *websocket-connections*))))
        (when on-close
          (funcall on-close ws))))
    (lambda (responder)
      (declare (ignore responder))
      (wsd:start-connection ws))))

(defun make-websocket-wrapper (app)
  "Wrap a Clack app to intercept WebSocket requests before they reach ningle.
Uses websocket-driver's websocket-p to detect upgrade requests."
  (lambda (env)
    (let ((path (getf env :path-info)))
      (if (and (gethash path *websocket-routes*)
               (wsd:websocket-p env))
          (handle-websocket-request env path)
          (funcall app env)))))
