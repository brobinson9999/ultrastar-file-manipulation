(defun read-file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (let ((total-result NIL) (this-result NIL))
      	(do () (NIL)
      		(setf this-result (read-line in NIL))
      		(unless this-result (return (format NIL "~{~a~^~%~}" (nreverse total-result))))
      		(push this-result total-result)
      	)))))

(defun read-file-form (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))
            	
(defun write-file (filename contents)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (format out contents))))
      

