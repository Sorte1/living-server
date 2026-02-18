(in-package :living-server)

(defvar *app* (make-instance 'ningle:app)
  "The ningle application instance. Persists across server stop/start.")

(setf (ningle:route *app* "/health")
      (lambda (params)
        (declare (ignore params))
        '(200
          (:content-type "application/json")
          ("{\"status\":\"ok\"}"))))

(defun eval-multi (code-string)
  "Evaluate multiple Lisp forms from a string, one at a time.
Each form is read and evaluated before the next is read.
This allows (ql:quickload ...) to make new packages available
before the reader encounters symbols from those packages."
  (handler-bind ((warning #'muffle-warning))
    (with-input-from-string (s code-string)
      (let ((eof (gensym)))
        (loop for form = (read s nil eof)
              until (eq form eof)
              for result = (eval form)
              finally (return result))))))

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
