
(asdf:defsystem #:ucl-job-searcher
  :description "Small project that will scan UCLs job board for interesting positions"
  :author "Ivan Kocienksi"
  :license "MIT"
  :serial t
  :depends-on (:drakma
	       :closure-html
	       :cxml
	       :css-selectors
	       :cl-ppcre
	       :split-sequence)
  
  :components ((:module "src"
			:components ((:file "package")
				     (:file "misc")
				     (:file "web")
				     (:file "wcn")
				     (:file "job-listing")
				     (:file "search")
				     (:file "main")))))

