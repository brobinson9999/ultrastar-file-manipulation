#!/usr/local/bin/clisp

(load "../cl-tap-producerX/cl-tap-framework.lisp")
(load "brute-io.lisp")
(load "split-sequence.lisp")

(load "../mk-defsystem/defsystem.lisp")

(load "../lispbuilder-regex/regex.translations")
(load "../lispbuilder-regex/regex.system")
(lc-regex)

(load "../lispbuilder-lexer/lexer.translations")
(load "../lispbuilder-lexer/lexer.system")
(lc-lexer)

(use-package :lispbuilder-lexer)

; an ultrastar-data is a plist. The following properties are recognized:
; title: (string) the title of the song (displayed in-game)
; artist: (string) the artist of the song (displayed in-game)
; mp3: (string) the filename of the mp3 that should be used
; bpm: (number) beats per minute
; gap: (number) milliseconds from the start of the MP3 to the first note
; cover: (string) the filename of a cover art file (JPG or PNG)
; background: (string) the filename of a background art file (JPG or PNG)
; relative: (boolean) This shows how the txt-file is written. If this tag is set to “yes”, the start of the notes is counted from zero in every line. If this tag is not available or set to “no”, the start of the notes is counted continuous from the beginning to the end of the txt-file.
; video: (string) the filename of a video file (avi, mpg, flv at least are supported)
; videogap: (number) the video file starts playing this many seconds from it's start
; start: (number) the MP3 file starts playing this many seconds from it's start
; genre: (string) genre of the song. this can be used for sorting songs, but has no other effect.
; edition: (string) what "edition" this song was ripped from. this can be used for sorting songs, but has no other effect.
; language: (string) language that the song is in. this can be used for sorting songs, but has no other effect.
; notes: (listof ultrastar-note-data) a list containing the body data of the ultrastar song

; an ultrastar-note-data is a (list note-type note-start note-length note-pitch lyrics) where:
; note-type is a symbol, one of :normal, :golden, :freestyle, or :line-break. Ultrastar files also support the note type "E" indicating end of file, but this is implicit and does not have a corresponding ultrastar-note-data.
; note-start is a number indicating the start time of the note. It is measured either from the beginning of the file or from the end of the previous line.
; note-length is a number indicating the length of the note.
; note-pitch is a number indicating the pitch of the note.
; lyrics is a string containing the lyrics for this note.

; parse-ultrastar-file: string -> ultrastar-data
; purpose: consumes a filename, and produces ultrastar-data representing
; the contents of that file.
(defun parse-ultrastar-file (file-name)
  (parse-ultrastar-string (read-file file-name)))

; parse-ultrastar-string: string -> ultrastar-data
; purpose: consumes a string representing in the format of
; an ultrastar song file, and produces structured ultrastar-data
; representing the contents of the file.
(defun parse-ultrastar-string (input-string)
  NIL)

; generate-ultrastar-header-data: ultrastar-data symbol string -> string
; purpose: consumes an ultrastar-data structure, the key of one of it's properties,
; and a string title for that property. If the property is non-NIL, a string
; containing it's textual representation is produced, otherwise an empty string
; is produced.
(defun generate-ultrastar-header-data (input-data header-key header-title)
  (if (getf input-data header-key)
      (format NIL "#~a:~a~%" header-title (getf input-data header-key))
    ""))

(is (generate-ultrastar-header-data '() :title "TITLE") (format NIL ""))
(is (generate-ultrastar-header-data '(:title "mytitle") :title "TITLE") (format NIL "#TITLE:mytitle~%"))
(is (generate-ultrastar-header-data '(:something "else") :title "TITLE") (format NIL ""))

(defun generate-ultrastar-note-string (input-data)
  ; probably should use destructuring-bind here
  (let ((note-type (nth 0 input-data))
	(note-start (nth 1 input-data))
	(note-length (nth 2 input-data))
	(note-pitch (nth 3 input-data))
	(lyrics (nth 4 input-data)))
    (case note-type
	  (:normal (format NIL ": ~a ~a ~a ~a" note-start note-length note-pitch lyrics))
	  (:golden (format NIL "* ~a ~a ~a ~a" note-start note-length note-pitch lyrics))
	  (:freestyle (format NIL "F ~a ~a ~a ~a" note-start note-length note-pitch lyrics))
	  (:line-break (format NIL "- ~a ~a" note-start note-length)))))

(is (generate-ultrastar-note-string '(:normal 1 2 3 "meow")) ": 1 2 3 meow")
(is (generate-ultrastar-note-string '(:golden 3 2 1 "gold")) "* 3 2 1 gold")
(is (generate-ultrastar-note-string '(:freestyle 3 2 1 "free")) "F 3 2 1 free")
(is (generate-ultrastar-note-string '(:line-break 3 2 1 "lb")) "- 3 2")

; generate-ultrastar-string: ultrastar-data -> string
; purpose: consumes structured ultrastar-data and produces a string representing
; that data in the format of an ultrastar song file.
(defun generate-ultrastar-string (input-data)
  (format NIL "~a~a~a~a~a~a~a~a~a~a~a~a~a~a~{~a~%~}E~%"
	  (generate-ultrastar-header-data input-data :title "TITLE")
	  (generate-ultrastar-header-data input-data :artist "ARTIST")
	  (generate-ultrastar-header-data input-data :mp3 "MP3")
	  (generate-ultrastar-header-data input-data :bpm "BPM")
	  (generate-ultrastar-header-data input-data :gap "GAP")
	  (generate-ultrastar-header-data input-data :cover "COVER")
	  (generate-ultrastar-header-data input-data :background "BACKGROUND")
	  (format NIL "#RELATIVE:~a~%" (if (getf input-data :relative) "YES" "NO"))
	  (generate-ultrastar-header-data input-data :video "VIDEO")
	  (generate-ultrastar-header-data input-data :videogap "VIDEOGAP")
	  (generate-ultrastar-header-data input-data :start "START")
	  (generate-ultrastar-header-data input-data :genre "GENRE")
	  (generate-ultrastar-header-data input-data :edition "EDITION")
	  (generate-ultrastar-header-data input-data :language "LANGUAGE")
	  (mapcar #'generate-ultrastar-note-string (getf input-data :notes))))

(is (generate-ultrastar-string '())
    (format NIL "#RELATIVE:NO~%E~%"))
(is (generate-ultrastar-string '(:title "mytitle"))
    (format NIL "#TITLE:mytitle~%#RELATIVE:NO~%E~%"))
(is (generate-ultrastar-string '(:irrelevant "text"))
    (format NIL "#RELATIVE:NO~%E~%"))
(is (generate-ultrastar-string '(:title "mytitle" :artist "myartist"))
    (format NIL "#TITLE:mytitle~%#ARTIST:myartist~%#RELATIVE:NO~%E~%"))
(is (generate-ultrastar-string '(:relative T))
    (format NIL "#RELATIVE:YES~%E~%"))
(is (generate-ultrastar-string '(:notes ((:normal 1 2 3 "yelp") (:golden 3 2 1 "bark"))))
    (format NIL "#RELATIVE:NO~%: 1 2 3 yelp~%* 3 2 1 bark~%E~%"))

; generate-ultrastar-file: string ultrastar-data -> string
; purpose: consumes a filename and an ultrastar-data, and produces a string
; containing the text of an ultrastar file storing that data. The text is
; also written to disk at the filename given.
(defun generate-ultrastar-file (file-name input-data)
  (write-file file-name (generate-ultrastar-string input-data)))

(deflexer example-lexer
    ("[0-9]+([.][0-9]+([Ee][0-9]+)?)"
      (return (values 'flt (num %0))))
    ("[0-9]+"
      (return (values 'int (int %0))))
    ("[:alpha:][:alnum:]*"
      (return (values 'name %0)))
    ("[:space:]+"))

(defparameter *exlex* (example-lexer "1.0 12 fred 10.23e5"))
;(is (multiple-value-list (funcall *exlex*)) '(flt 1.0))
;(is (multiple-value-list (funcall *exlex*)) '(int 12))
;(is (multiple-value-list (funcall *exlex*)) '(name "fred"))
;(is (multiple-value-list (funcall *exlex*)) '(flt 10.23e5))
;(is (multiple-value-list (funcall *exlex*)) '(NIL NIL))


(print-test-plan)