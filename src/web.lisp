(in-package :ucl-job-search)

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

(defun node-value (node)
  (dom:node-value (aref (dom:child-nodes node) 0)))

(defun extract-all-tag-text (dom)
  (format nil "~{~a ~}"
	  (loop for child across (dom:child-nodes dom)
	     collect (cond
		       ((typep child 'dom:text) (dom:node-value child))
		       (t (extract-all-tag-text child))))))
