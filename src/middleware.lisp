(in-package :cas-middleware)

(defvar *cas-client*)

(defclass cas-client ()
  ((cas-server-url :accessor cas-server-url
		   :initarg :cas-server-url)
   (url :accessor app-url
	:initarg :app-url)
   (logout-url :accessor app-logout-url
               :initarg :app-logout-url)
   (disable-sso :accessor app-disable-sso
		:initarg :app-disable-sso
		:initform nil)
   (excludes :accessor app-excludes
	     :initarg :app-excludes)))

(defmethod route-excludedp ((cas cas-client) route)
  (find route (app-excludes cas)
        :test #'(lambda (pattern) (ppcre:scan-to-strings (format nil "^~a.*$" pattern) route))))

(defmethod cas-login ((cas cas-client) quri)
  (let ((request-path (quri:uri-path quri))
	(request-query (quri:uri-query quri)))
   (return-302 (cas-login-url (cas-server-url cas)
                                  :service-url (format-url (app-url cas) request-path :query request-query)
                                  :renew (app-disable-sso cas)))))

(defmethod cas-validate-ticket ((cas cas-client) quri ticket)
  (let ((request-path (quri:uri-path quri))
	(request-query (remove-ticket-from-query (quri:uri-query-params quri))))
    (cas-service-validate (cas-server-url cas)
			      (format-url (app-url cas) request-path :query request-query)
			      ticket)))

(defmethod cas-logout ((cas cas-client) &optional url)
  (return-302 (cas-logout-url (cas-server-url cas) :url (if url url (app-logout-url cas)))))

(defun logout (session &optional url)
  (remhash :session-id session)
  (remhash :user-uid session)
  (remhash :user-attrs session)
  (cas-logout *cas-client* url))

(defun remove-ticket-from-query (query-params)
  (remove "ticket" query-params :test #'string= :key #'first))

;;
;;
;;

(defun request-uri (env)
  (quri:make-uri :scheme (getf env :url-scheme)
		 :host (getf env :server-name)
		 :port (getf env :server-port)
		 :path (getf env :path-info)
		 :query (getf env :query-string)))

(defun cookie-session-id (env)
  (cdr (assoc "lack.session" (getf env :cookies) :test #'string=)))

(defun stored-session-id (env)
  (gethash :session-id (getf env :lack.session) nil))

(defun cas-authenticated-user (session)
  (values (gethash :user-uid session nil) (gethash :user-attrs session nil)))

(defun authenticatedp (env)
  "Check if Cookie Session ID is set and equal to stored session ID"
  (when-let ((stored-session-id (stored-session-id env)))
    (let ((cookie-session-id (cookie-session-id env)))
      (string= cookie-session-id stored-session-id))))

(defun session-store-cas-user (env uid &optional (attributes ()))
  (setf (gethash :user-uid (getf env :lack.session) nil) uid)
  (when attributes
    (setf (gethash :user-attrs (getf env :lack.session) nil) attributes)))

(defun session-store-cas-session-id (env)
  (setf (gethash :session-id (getf env :lack.session) nil) (cookie-session-id env)))

(defun auth-cas-ticket (query)
  "If exists, return cas-ticket from URL"
  (and query (cas:cas-ticket query)))

(defun allow-user-request (quri env uid &optional (attributes ()))
  "Stores user data returned by CAS server in the sessions.
   Then redirects the user to the requested page.
   The redirection is necessary to avoid displaying the CAS ticket in the URL (See CAS Protocol)."
  (session-store-cas-user env uid attributes)
  (session-store-cas-session-id env)
  
  (return-302 (format-url (app-url *cas-client*)
			  (quri:uri-path quri)
			  :query (remove-ticket-from-query (quri:uri-query-params quri)))))



(defparameter *lack-middleware-auth-cas*
  (lambda (app &key config)
    (lambda (env)
      (let ((*cas-client* (apply #'make-instance 'cas-client config))
	    (uri (request-uri env)))
       (if (route-excludedp *cas-client* (quri:uri-path uri))
	   (funcall app env)
	   (if (authenticatedp env)
	       (funcall app env)
	       (if-let ((ticket (auth-cas-ticket (quri:uri-query uri))))
		 (multiple-value-bind (username attributes)
		     (cas-validate-ticket *cas-client* uri ticket)
		   (if username
                       (allow-user-request uri env username attributes)
                       (return-500)))
		 (cas-login *cas-client* uri)))))))
    "Middleware for CAS Authentication")

(defun return-500 ()
  `(500
    (:content-type "text/plain"
     :content-length 52)
    ("Internal Server Error : Cas ticket validation failed")))

(defun return-302 (url)
  `(302 (:Location ,url)))

(defun return-401 ()
  `(401 (:content-type "text/plain"
         :content-length 12)
        ("Unauthorized")))


