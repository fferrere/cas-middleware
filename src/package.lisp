(defpackage cas-middleware
  (:use :cl :cl-cas :alexandria)
  (:nicknames :lack.middleware.auth.cas :cm)
  (:export :*lack-middleware-auth-cas*
           :cas-authenticated-user
           :logout))

