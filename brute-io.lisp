(defun read-file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (let ((line-list NIL) (this-result NIL))
      	(do () (NIL)
	    (multiple-value-bind (line eof) (read-line in NIL)
				 (when line (push line line-list))
				 (unless line (return (format NIL "~{~a~%~}" (nreverse line-list))))
				 (when eof (return (format NIL "~{~a~^~%~}" (nreverse line-list))))
				 ))))))

(defun read-file-form (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))
            	
(defun write-file (filename contents)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (format out "~a" contents))))
      
