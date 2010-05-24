#!/usr/local/bin/clisp

(load "ultrastar-file-manipulation.lisp")

(defvar *filename* "Pornophonique - Space Invaders.txt")
(defvar *file* (parse-ultrastar-file *filename*))

(defun bump-pitch (pitch-adjustment)
  (setf (getf *file* :notes)
	(loop for note in (getf *file* :notes)
	      collecting   (destructuring-bind
			    (note-type note-start note-length note-pitch lyrics) note
			    (list note-type note-start note-length (+ note-pitch pitch-adjustment) lyrics)))))

(bump-pitch 1)

(write-file (format NIL "new~a" *filename*)
	    (generate-ultrastar-string *file*))
