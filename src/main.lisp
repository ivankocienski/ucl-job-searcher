(in-package :ucl-job-search)

(defparameter *interesting-keywords* '("ruby" "python" "java" "c++" "library"))

(defun pull-interesting-listings ()
  (with-cookie-jar (cookie-jar)
    (let ((job-listings (get-search-page cookie-jar)))
      (format t "Found ~d listings today~%" (length job-listings))

      (fetch-search-pages job-listings cookie-jar)

      (sort job-listings (lambda (x y)
			   (local-time:timestamp> (job-listing-ends-at x)
						  (job-listing-ends-at y))))
      
      (dolist (job job-listings)
	(let ((counts (count-words *interesting-keywords* (extract-text job))))
	  (when (> (hash-table-count counts) 0)
	    (format t "----------~%")
	    (format t "         : ~a~%" (job-listing-title job))
	    (format t "      in : ~a~%" (job-listing-department job))
	    (format t "    ends : ~a~%" (job-listing-ends-at job))
	    (format t "keywords : ~{~a ~}~%" (loop for key being the hash-keys of counts
						collect (format nil "~a(~d)" key
								(gethash key counts))))))))))

(defun main ()
  (pull-interesting-listings))
