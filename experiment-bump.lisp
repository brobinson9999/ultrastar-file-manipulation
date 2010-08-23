;#!/usr/bin/clisp

(load "ultrastar-file-manipulation.lisp")

(defparameter *filename* "Britney Spears - Lucky.txt")
(defparameter *file* (parse-ultrastar-file *filename*))

(defun bump-pitch (pitch-adjustment)
  (setf (getf *file* :notes)
	(loop for note in (getf *file* :notes)
	      collecting   (destructuring-bind
			    (note-type note-start note-length note-pitch lyrics) note
			    (list note-type note-start note-length (+ note-pitch pitch-adjustment) lyrics)))))


(getf *file* :video)
;(bump-pitch 1)

;(write-file *filename*
;	    (generate-ultrastar-string *file*))
