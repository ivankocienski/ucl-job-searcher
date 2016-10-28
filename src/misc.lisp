(in-package :ucl-job-search)

(defparameter *curdir* (asdf:system-source-directory :ucl-job-searcher))

(defun file-exists? (path-string)
  (handler-case
      (let ((stat (sb-posix:stat path-string)))
	;;(format t "file stat= ~d~%" (sb-posix:stat-mode stat))
	t)
    (sb-posix:syscall-error () nil)))

(defconstant +MONTH-NAMES+ '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

(defun date-parse (text)
  (labels ((month-to-integer (name)
	     (1+ (position (string-downcase name)
			   +MONTH-NAMES+
			   :test #'string=))))
    
    (let ((parts (split-sequence:split-sequence #\Space text)))
      (local-time:encode-timestamp 0 ;;nsec
				   0 ;; sec
				   0 ;; minute
				   0 ;; hour
				   (parse-integer (first parts)) ;; day
				   (month-to-integer (second parts)) ;; month
				   (parse-integer (third parts)))))) ;; year
