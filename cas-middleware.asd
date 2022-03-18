(defsystem "cas-middleware" 
  :version "0.1"
  :author "Frédéric FERRERE"
  :license "Apache-2.0"
  :description "Lack MiddleWare Authentication CAS package"
  :depends-on ("alexandria" "cl-ppcre" "cl-cas" "split-sequence" "quri")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "middleware")))))
