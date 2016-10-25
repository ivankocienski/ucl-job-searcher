(in-package :ucl-job-search)

(defparameter *curdir* (asdf:system-source-directory :ucl-job-searcher))

(defparameter *landing-url* "https://atsv7.wcn.co.uk/search_engine/jobs.cgi?SID=b3duZXI9NTA0MTE3OCZvd25lcnR5cGU9ZmFpciZwb3N0aW5nX2NvZGU9MjI0")
(defparameter *search-url* "https://atsv7.wcn.co.uk/search_engine/jobs.cgi")

(defparameter *interesting-keywords* '("ruby" "python" "java" "c++" "library"))

(defmacro do-get ((body-var url cookie-jar) &body body)
  (let ((status-var  (gensym "status"))
	(headers-var (gensym "headers")))
    `(multiple-value-bind (,body-var
			   ,status-var
			   ,headers-var) (drakma:http-request ,url
							      :cookie-jar ,cookie-jar)
       (declare (ignore ,headers-var))
       (if (= ,status-var 200)
	   (progn ,@body)
	   (error (format nil "GET '~s' failed (status=~d)" ,url ,status-var))))))

(defmacro do-post ((body-var url fields cookie-jar) &body body)
  (let ((status-var  (gensym "status"))
	(headers-var (gensym "headers")))
    `(multiple-value-bind (,body-var
			   ,status-var
			   ,headers-var) (drakma:http-request ,url
							      :method :post
							      :parameters ,fields
							      :cookie-jar ,cookie-jar)
       (declare (ignore ,headers-var))
       (if (= ,status-var 200)
	   (progn ,@body)
	   (error (format nil "POST '~s' failed (status=~d)" ,url ,status-var))))))

(defmacro with-dom ((dom-var body-input-var) &body body)
  `(let ((,dom-var (chtml:parse ,body-input-var (cxml-dom:make-dom-builder))))
     ,@body))
    
(defun site-cookie-jar ()
  (let ((cookie-jar (make-instance 'drakma:cookie-jar)))
    (do-get (body *landing-url* cookie-jar)
      cookie-jar)))

(defmacro with-cookie-jar ((cookie-jar-var) &body body)
  `(let ((,cookie-jar-var (site-cookie-jar)))
     ,@body))

(defun pull-out-search-fields (dom)
  (loop for inp in (css:query "form input" dom)
     when (string= (dom:get-attribute inp "type") "hidden")
     collect (cons (dom:get-attribute inp "name")
		   (dom:get-attribute inp "value"))))

(defun data-row? (row)
  (string= (dom:node-name (aref (dom:child-nodes row) 0)) "td"))

(defstruct job-listing
  title
  link
  department
  ends-at)

(defparameter *listing-id-extractor-re* (cl-ppcre:create-scanner "SID=(.*)"))

(defun job-listing-id (jl)
  (multiple-value-bind (found
			captures) (cl-ppcre:scan-to-strings *listing-id-extractor-re*
							    (job-listing-link jl))
      (when found
	(subseq (aref captures 0) 0 60))))

(defun job-listing-path (jl)
  (format nil "~a/data/~a.html"
	  *curdir*
	  (job-listing-id jl))
  )

(defun node-value (node)
  (dom:node-value (aref (dom:child-nodes node) 0)))

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
      

(defun file-exists? (path-string)
  (handler-case
      (let ((stat (sb-posix:stat path-string)))
	;;(format t "file stat= ~d~%" (sb-posix:stat-mode stat))
	t)
    (sb-posix:syscall-error () nil)))

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

(defun extract-all-tag-text (dom)
  (format nil "~{~a ~}"
	  (loop for child across (dom:child-nodes dom)
	     collect (cond
		       ((typep child 'dom:text) (dom:node-value child))
		       (t (extract-all-tag-text child))))))

(defun extract-text (from-job)
  (let ((path (job-listing-path from-job)))
    (with-open-file (file path)
      (let ((contents (format nil "~{~a~}"
			      (loop for line = (read-line file nil nil)
				 while line
				 collect line))))

	;;(format t "len=~d~%" (length contents))
	(let* ((dom (chtml:parse contents (cxml-dom:make-dom-builder)))
	       (content-div (css:query1 ".vac_desc" dom)))

	  (extract-all-tag-text content-div))))))
	  
	  

;; this is very crude			     
(defun count-words (in-hunt-words in-string)
  (let ((hunt-word-list (mapcar #'(lambda (w) (cons (string-downcase w)
						    (length w)))
				in-hunt-words))
	(in-string (string-downcase in-string))
	(tally (make-hash-table)))

    (dolist (word (split-sequence:split-sequence #\Space in-string))
      (dolist (hunt hunt-word-list)
	(let ((hunt-word (car hunt))
	      (hunt-len  (cdr hunt)))
	  
	  (when (>= (length word) hunt-len)
	    
	    (let ((test-word (subseq word 0 hunt-len)))
	      (when (string= test-word hunt-word)
		(incf (gethash hunt-word tally 0))))))))
    tally))

(defun show-word-counts (hunt-words in-string)
  (let ((counts (count-words hunt-words in-string)))
    (loop for key being the hash-keys of counts
       do (let ((tally (gethash key counts)))
	    (format t "'~a'=~d~%" key tally)))))

(defun pull-interesting-listings ()
  (with-cookie-jar (cookie-jar)
    (let ((job-listings (get-search-page cookie-jar)))
      (format t "Found ~d listings today~%" (length job-listings))

      (fetch-search-pages job-listings cookie-jar)

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
