# CAS-MIDDLEWARE

Add CAS authentication to [Caveman](https://github.com/fukamachi/caveman) Web Application.

Use CL-CAS system (https://github.com/fferrere/cl-cas)

Provide :
- Authentication by default for the entire application
- Access to CAS user name and attribtues
- CAS Logout

## Installation
1. git clone https://github.com/fferrere/cl-cas
2. git clone https://github.com/fferrere/cas-middleware
3. Add cas-middleware system to your Caveman project (asd file) `:depends-on ("cas-middleware" ...)`
4. Add config data to <Your Caveman Project>/src/config.lisp
5. Add middleware to <Your Caveman Project>/app.lisp

## System Nicknames
- lack.middleware.auth.cas : mandatory for caveman
- cm : to use cas-logout and cas-authenticated-user functions

## Usage

By default all routes requires authenticated session, but you can exclude some routes, like a Welcome page.

### Configuration

- [Required] :cas-server-url, URL of the CAS server
- [Required] :app-url, URL of your app. Used when your app is behind a proxy
- [Optional] :app-logout-url, URL sent to the CAS server during logout. The cas server may then redirect to this URL.
- [Optional] :app-disable-sso, if t disable single sign-on (SSO). Default is nil = SSO on.
- [Optional] :app-excludes, Excludes path URI from CAS authentication (public pages)

#### Method 1 : Add to Caveman config.lisp file

Example (SSO : On - No excludes routes) :
```
(defconfig |development|
    `(:cas (:cas-server-url "https://casserver.herokuapp.com/cas"
            :app-url "http://localhost:5000")))
```

Example (SSO : On - Excludes routes) :
```
(defconfig |development|
    `(:cas (:cas-server-url "https://casserver.herokuapp.com/cas"
            :app-url "http://localhost:5000"
	    :app-excludes ("/"))))
```

Example (SSO Off, force authentication - No excludes routes) :
```
(defconfig |development|
    `(:cas (:cas-server-url "https://casserver.herokuapp.com/cas"
            :app-disable-sso t))))
```

#### Method 2 : Use ENV vars and uiop:getenv

- CAS_URL : The cas server URL, default to "https://casserver.herokuapp.com/cas"
- APP_URL : The CAS-DEMO web app URL. Default to "http://localhost:5000"
- APP_LOGOUT_URL : The CAS-DEMO logout URL. Default to "http://localhost/demo/logout"
- APP_EXCLUDES : List of paths of the web app excluded from CAS authentication (public pages). Default  "/ /logout"

Example into Caveman config.lisp file :

```
(defconfig |production|
    `(:cas (:cas-server-url ,(uiop:getenv "CAS_URL")
	    :app-url ,(uiop:getenv "APP_URL")
	    :app-logout-url ,(uiop:getenv "APP_LOGOUT_URL")
	    :app-excludes ,(uiop:split-string (uiop:getenv "APP_EXCLUDES")))))
```

### Add CAS-Middleware component to your app (app.lisp file)

Exemple :

```
(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 (if (productionp)
     nil
     :accesslog)
 (if (getf (config) :error-log)
     `(:backtrace
       :output ,(getf (config) :error-log))
     nil)
 :session
 (if (productionp)
     nil
     (lambda (app)
       (lambda (env)
         (let ((datafly:*trace-sql* t))
           (funcall app env)))))
(:auth-cas :config (getf (config) :cas))
 *web*)
```

### User name and attributes

CAS-Middleware stores user name and attributes into Caveman session. To get this data, use :
```
(multiple-value-bind (username attributes) (cas-authenticated-user *session*)
  <body>
  )
```

### CAS Logout

`(cas-logout *session*)`

If redirect-url is supported bye the server

`(cas-logout *session* redirect-url)`


## Demo

See https://github.com/fferrere/cas-demo

## Author

* Frédéric FERRERE (frederic.ferrere@gmail.com)

## Licence

Apache-2.0 (https://www.apache.org/licenses/LICENSE-2.0)


