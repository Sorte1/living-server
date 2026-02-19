(defpackage :living-server
  (:use :cl)
  (:export #:start-all
           #:stop-all
           #:start-server
           #:stop-server
           #:start-swank
           #:*app*
           #:*server*
           #:list-routes
           #:eval-multi
           #:server-status
           ;; WebSocket
           #:add-websocket-route
           #:remove-websocket-route
           #:websocket-broadcast
           #:websocket-send
           #:*websocket-routes*))
