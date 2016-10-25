(in-package :ucl-job-search)

(defparameter *landing-url* "https://atsv7.wcn.co.uk/search_engine/jobs.cgi?SID=b3duZXI9NTA0MTE3OCZvd25lcnR5cGU9ZmFpciZwb3N0aW5nX2NvZGU9MjI0")
(defparameter *search-url* "https://atsv7.wcn.co.uk/search_engine/jobs.cgi")

(defun site-cookie-jar ()
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
    (do-get (body *landing-url* cookie-jar)
      cookie-jar)))

(defmacro with-cookie-jar ((cookie-jar-var) &body body)
  `(let ((,cookie-jar-var (site-cookie-jar)))
     ,@body))
