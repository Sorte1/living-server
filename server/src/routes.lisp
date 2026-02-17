(in-package :living-server)

(defvar *app* (make-instance 'ningle:app)
  "The ningle application instance. Persists across server stop/start.")

(setf (ningle:route *app* "/health")
      (lambda (params)
        (declare (ignore params))
        '(200
          (:content-type "application/json")
          ("{\"status\":\"ok\"}"))))

(defun list-routes ()
  "Return a list of all registered routes as ((methods...) path) lists."
  (let* ((mapper (slot-value *app* 'ningle/app::mapper))
         (routes-fn (myway.mapper::mapper-%routes mapper))
         (routes (funcall routes-fn))
         (result '()))
    (dolist (r routes)
      (let* ((rule (slot-value r 'myway.route::rule))
             (methods-set (myway.rule::rule-methods rule))
             (methods '()))
        (map-set:ms-for-each
         (lambda (m) (push (string m) methods))
         methods-set)
        (push (list (nreverse methods) (myway.rule::rule-url rule))
              result)))
    (nreverse result)))
