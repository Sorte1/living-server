(defsystem "living-server"
  :description "A living Common Lisp web server that accepts hot-loaded code"
  :version "0.1.0"
  :depends-on ("ningle"
               "clack"
               "woo"
               "swank"
               "alexandria"
               "jonathan"
               "str"
               "cl-ppcre"
               "local-time"
               "bordeaux-threads")
  :serial t
  :components ((:module "src"
                :components ((:file "packages")
                             (:file "config")
                             (:file "routes")
                             (:file "swank")
                             (:file "server")))))
