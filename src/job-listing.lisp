(in-package :ucl-job-search)

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
	  (job-listing-id jl)))

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
