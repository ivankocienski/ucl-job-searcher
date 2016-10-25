(in-package :ucl-job-search)

(defparameter *curdir* (asdf:system-source-directory :ucl-job-searcher))

(defun file-exists? (path-string)
  (handler-case
      (let ((stat (sb-posix:stat path-string)))
	;;(format t "file stat= ~d~%" (sb-posix:stat-mode stat))
	t)
    (sb-posix:syscall-error () nil)))
