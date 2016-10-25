(in-package :ucl-job-search)

(defun pull-out-search-fields (dom)
  (loop for inp in (css:query "form input" dom)
     when (string= (dom:get-attribute inp "type") "hidden")
     collect (cons (dom:get-attribute inp "name")
		   (dom:get-attribute inp "value"))))

(defun data-row? (row)
  (string= (dom:node-name (aref (dom:child-nodes row) 0)) "td"))



(defun data-pullout (row)
  (let ((anchor (css:query1 "tr td:nth-child(1) a" row))
	(department (css:query1 "tr td:nth-child(2)" row))
	(ends-at (css:query1 "tr td:nth-child(4)" row)))
    
    (make-job-listing :title      (node-value anchor)
		      :link       (dom:get-attribute anchor "href")
		      :department (node-value department)
		      :ends-at    (node-value ends-at))))

(defun extract-search-results (body)
  (with-dom (result-dom body)
    (loop for row in (css:query "table tr" result-dom)
       when (data-row? row)
       collect (data-pullout row))))

(defun get-search-page (cookie-jar)
  (do-get (body *landing-url* cookie-jar)
    (with-dom (start-dom body)
      (do-post (search-body *search-url* (pull-out-search-fields start-dom) cookie-jar)
	(extract-search-results search-body)))))

(defun fetch-search-pages (listing cookie-jar)
  (let ((fetch-links (delete-if #'(lambda (job)
				    (file-exists? (job-listing-path job)))
				    listing)))

    (format t "pulling ~d pages~%" (length fetch-links))	
						  
    (dolist (job fetch-links)
      (format t ".")
      (let ((job-path (job-listing-path job)))
	(with-open-file (file job-path :direction :output)
	  (do-get (page (job-listing-link job) cookie-jar)
	    (format file "~a" page)))))))
