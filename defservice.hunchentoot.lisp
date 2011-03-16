#-hunchentoot(error "defservice.hunchentoot is Hunchentoot-specific code")

(defpackage :defservice.hunchentoot
  (:use :defservice :cl :hunchentoot)
  (:export #:hunchentoot-param-reader #:create-context-dispatcher)
  (:shadowing-import-from :hunchentoot :url-encode :url-decode))

(in-package :defservice.hunchentoot)

(defun create-context-dispatcher (prefix context)
  (unless (and (stringp prefix)
               (plusp (length prefix))
               (char/= (char prefix (1- (length prefix))) #\/))
    (parameter-error "~S must be string that doesn't end with a slash." prefix))
  (unless (symbolp context)
    (parameter-error "~S must be a symbol denoting a context." context))
  (lambda (request)
    (let* ((name (script-name request))
           (mismatch (mismatch name prefix :test #'char=)))
      (and (or (null mismatch)
               (>= mismatch (length prefix)))
           (lambda ()
             (handler-case
                 (dispatch-request context (request-method request)
                                   (if mismatch
                                       (subseq name mismatch)
                                       "/")
                                   (hunchentoot-param-reader request))
               (dispatch-failed (err)
                 (setf (return-code*) (dispatch-failed-code err)))))))))

(defun hunchentoot-param-reader (request)
  (lambda (name type &optional (default nil default-p))
    (case type
      (:body (or (raw-post-data :request request :external-format :utf-8 :force-text t) ""))
      (:body-binary (raw-post-data :request request :force-binary t))
      (:content-type (let* ((ct (header-in :content-type request))
                            (sc (position #\; ct)))
                       (if sc (subseq ct 0 sc) ct)))
      (:request request)
      (:method (request-method request))
      (:list (loop :for (nm . val) :in (get-parameters request)
                   :when (string= nm name) :collect val))
      (:vars (loop :for param :in (get-parameters request)
                   :when (char= (char (car param) 0) #\$) :collect param))
      (t (let ((str (parameter name request)))
           (cond (str
                  (multiple-value-bind (value ok) (read-parameter-type type str)
                    (unless ok
                      (error 'dispatch-failed :code 400
                             :message (format nil "'~a' is not a valid ~a." str type)))
                    value))
                 (default-p default)
                 (t (error 'dispatch-failed :code 400
                           :message (format nil "No value given for parameter '~a'." name)))))))))

(defgeneric read-parameter-type (type string)
  (:method ((type (eql :string)) string)
    (values string t))
  (:method ((type (eql :boolean)) string)
    (if (string= string "true") (values t t) (values nil t)))
  (:method ((type (eql :integer)) string)
    (handler-case (values (parse-integer string) t)
      (error () nil))))
