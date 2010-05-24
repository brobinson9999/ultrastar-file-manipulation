(load "brute-io.lisp")
(load "split-sequence.lisp")

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

; generate-ultrastar-header-data: ultrastar-data symbol string -> string
; purpose: consumes an ultrastar-data structure, the key of one of it's properties,
; and a string title for that property. If the property is non-NIL, a string
; containing it's textual representation is produced, otherwise an empty string
; is produced.
(defun generate-ultrastar-header-data (input-data header-key header-title)
  (if (getf input-data header-key)
      (format NIL "#~a:~a~%" header-title (getf input-data header-key))
    ""))

; generate-ultrastar-note-string: ultrastar-note-data -> string
; purpose: consumes structured ultrastar-note-data and produces a string representing
; that data in the format of an ultrastar song file.
(defun generate-ultrastar-note-string (input-data)
  (destructuring-bind
   (note-type note-start note-length note-pitch lyrics) input-data
   (case note-type
	 (:normal (format NIL ": ~a ~a ~a ~a" note-start note-length note-pitch lyrics))
	 (:golden (format NIL "* ~a ~a ~a ~a" note-start note-length note-pitch lyrics))
	 (:freestyle (format NIL "F ~a ~a ~a ~a" note-start note-length note-pitch lyrics))
	 (:line-break (if (= note-length 0)
			  (format NIL "- ~a" note-start)
			(format NIL "- ~a ~a" note-start note-length))))))

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

; generate-ultrastar-file: string ultrastar-data -> string
; purpose: consumes a filename and an ultrastar-data, and produces a string
; containing the text of an ultrastar file storing that data. The text is
; also written to disk at the filename given.
(defun generate-ultrastar-file (file-name input-data)
  (write-file file-name (generate-ultrastar-string input-data)))

; string-starts-with: string string -> boolean
; purpose: produces true if the second string starts with the
; same characters as the first string.
(defun string-starts-with (start-string input-string)
  (cond ((< (length input-string) (length start-string)) NIL)
	(T (equalp start-string (subseq input-string 0 (length start-string))))))

; string-chomp: string string -> string
; purpose: consumes a string to chomp and a string to chomp
; it from. Produces the second string with the length of the
; first string removed from it's start. The second string does
; not have to start with the first string, only the length of
; the first string is used. If the length of the first string
; is greater than the length of the second string, the empty
; string is produced.
(defun string-chomp (start-string input-string)
  (cond ((<= (length input-string) (length start-string)) "")
	(T (subseq input-string (length start-string) (length input-string)))))

; a condition raised when a line of a song file is not valid
(define-condition invalid-song-file-line (error)
  ((text :initarg :text :reader text)))

; parse-ultrastar-notes: (listof string) -> (listof ultrastar-note-data)
; purpose: consumes a list of strings in the Ultrastar song file format,
; consisting only of notes, and produces a list of ultrastar-note-data
; representing the same notes
(defun parse-ultrastar-notes (input-list)
  (cond
   ((eql input-list NIL) NIL)
   ((string-starts-with "E" (first input-list)) NIL)
   (T (cons (parse-ultrastar-note (first input-list)) (parse-ultrastar-notes (rest input-list))))))

; parse-ultrastar-note: string -> ultrastar-note-data
; purpose: consumes a string representing a single note
; in the Ultrastar song file format, and produces the
; ultrastar-note-data representing that same note
(defun parse-ultrastar-note (note-string)
  (let ((note-list (split-sequence:split-sequence #\Space note-string)))
    (list (cond ((equalp (first note-list) ":") :normal)
		((equalp (first note-list) "*") :golden)
		((equalp (first note-list) "F") :freestyle)
		((equalp (first note-list) "-") :line-break)
		(T (error 'invalid-song-file-line :text (first note-list))))
	  (if (second note-list) (parse-integer (second note-list)) 0)
	  (if (third note-list) (parse-integer (third note-list)) 0)
	  (if (fourth note-list) (parse-integer (fourth note-list)) 0)
	  (format NIL "~{~a~^ ~}" (cddddr note-list)))))

; parse-ultrastar-lines: (listof string) -> ultrastar-data
; purpose: consumes a list of strings in the format of
; an ultrastar song file, and produces structured ultrastar-data
; representing the contents of the file. Each element of the input
; list is one line of the song file.
(defun parse-ultrastar-lines (input-list)
  (cond
   ((eql input-list NIL) NIL)
   ((string-starts-with "E" (first input-list)) NIL)
   ((string-starts-with "#TITLE:" (first input-list))
    (add-str-prop-and-recurse "#TITLE:" :title input-list))
   ((string-starts-with "#ARTIST:" (first input-list))
    (add-str-prop-and-recurse "#ARTIST:" :artist input-list))
   ((string-starts-with "#MP3:" (first input-list))
    (add-str-prop-and-recurse "#MP3:" :mp3 input-list))
   ((string-starts-with "#BPM:" (first input-list))
    (add-num-prop-and-recurse "#BPM:" :bpm input-list))
   ((string-starts-with "#GAP:" (first input-list))
    (add-num-prop-and-recurse "#GAP:" :gap input-list))
   ((string-starts-with "#COVER:" (first input-list))
    (add-str-prop-and-recurse "#COVER:" :cover input-list))
   ((string-starts-with "#BACKGROUND:" (first input-list))
    (add-str-prop-and-recurse "#BACKGROUND:" :background input-list))
   ((string-starts-with "#GENRE:" (first input-list))
    (add-str-prop-and-recurse "#GENRE:" :genre input-list))
   ((string-starts-with "#RELATIVE:" (first input-list))
    (add-bool-prop-and-recurse "#RELATIVE:" :relative input-list))
   ((string-starts-with "#EDITION:" (first input-list))
    (add-str-prop-and-recurse "#EDITION:" :edition input-list))
   ((string-starts-with "#LANGUAGE:" (first input-list))
    (add-str-prop-and-recurse "#LANGUAGE:" :language input-list))
   ((string-starts-with "#VIDEO:" (first input-list))
    (add-num-prop-and-recurse "#VIDEO:" :video input-list))
   ((string-starts-with "#VIDEOGAP:" (first input-list))
    (add-num-prop-and-recurse "#VIDEOGAP:" :videogap input-list))
   ((string-starts-with "#START:" (first input-list))
    (add-num-prop-and-recurse "#START:" :start input-list))
   ((or (string-starts-with ":" (first input-list))
	(string-starts-with "*" (first input-list))
	(string-starts-with "F" (first input-list))
	(string-starts-with "-" (first input-list)))
    (list :notes (parse-ultrastar-notes input-list)))
   (T (error 'invalid-song-file-line :text (first input-list)))))

(defun add-str-prop-and-recurse (string-start keyword input-list)
  (cons keyword
	(cons (string-chomp string-start (first input-list))
	      (parse-ultrastar-lines (rest input-list)))))

; Allows junk because BPM sometimes has a comma and a part after the comma. I haven't been able to find any
; documentation for what that comma is about. I'm currently throwing it out.
(defun add-num-prop-and-recurse (string-start keyword input-list)
  (cons keyword
	(cons (parse-integer (string-chomp string-start (first input-list)) :junk-allowed T)
	      (parse-ultrastar-lines (rest input-list)))))

(defun add-bool-prop-and-recurse (string-start keyword input-list)
  (cons keyword
	(cons (eql (string-chomp string-start (first input-list)) "yes")
		      (parse-ultrastar-lines (rest input-list)))))

; parse-ultrastar-string: string -> ultrastar-data
; purpose: consumes a string representing in the format of
; an ultrastar song file, and produces structured ultrastar-data
; representing the contents of the file.
(defun parse-ultrastar-string (input-string)
  (parse-ultrastar-lines (split-sequence:split-sequence #\Newline input-string)))

; parse-ultrastar-file: string -> ultrastar-data
; purpose: consumes a filename, and produces ultrastar-data representing
; the contents of that file.
(defun parse-ultrastar-file (file-name)
  (parse-ultrastar-string (read-file file-name)))
