#!/usr/local/bin/clisp

(load "ultrastar-file-manipulation.lisp")
(load "../cl-tap-producerX/cl-tap-framework.lisp")

(is (generate-ultrastar-header-data '() :title "TITLE") (format NIL ""))
(is (generate-ultrastar-header-data '(:title "mytitle") :title "TITLE") (format NIL "#TITLE:mytitle~%"))
(is (generate-ultrastar-header-data '(:something "else") :title "TITLE") (format NIL ""))

(is (generate-ultrastar-note-string '(:normal 1 2 3 "meow")) ": 1 2 3 meow")
(is (generate-ultrastar-note-string '(:golden 3 2 1 "gold")) "* 3 2 1 gold")
(is (generate-ultrastar-note-string '(:freestyle 3 2 1 "free")) "F 3 2 1 free")
(is (generate-ultrastar-note-string '(:line-break 3 2 1 "lb")) "- 3 2")
(is (generate-ultrastar-note-string '(:line-break 3 0 0 "")) "- 3")

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

(is (string-starts-with "" "") T)
(is (string-starts-with "a" "a") T)
(is (string-starts-with "ab" "abab") T)
(is (string-starts-with "abab" "baba") NIL)
(is (string-starts-with "baba" "bab") NIL)

(is (string-chomp "" "") "")
(is (string-chomp "a" "a") "")
(is (string-chomp "a" "ab") "b")
(is (string-chomp "aaa" "b") "")
(is (string-chomp "abab" "ababab") "ab")

(is (parse-ultrastar-note ":") '(:normal 0 0 0 ""))
(is (parse-ultrastar-note "F 0 4 20 I") '(:freestyle 0 4 20 "I"))
(is (parse-ultrastar-note ": 0 4 20 I") '(:normal 0 4 20 "I"))
(is (parse-ultrastar-note ": 0 4 20 I just") '(:normal 0 4 20 "I just"))
(is (parse-ultrastar-note "* 20 24 200 I just") '(:golden 20 24 200 "I just"))

(is-condition (parse-ultrastar-note "WTF? 0 4") (make-condition 'invalid-song-file-line :text "WTF?"))

(is (parse-ultrastar-lines '("#ARTIST:Joe Blow")) '(:artist "Joe Blow"))
(is (parse-ultrastar-lines '("#ARTIST:Joe Blow"
			     "#TITLE:Country Blues")) '(:artist "Joe Blow" :title "Country Blues"))
(is (parse-ultrastar-lines '("#ARTIST:Joe Blow"
			     "#TITLE:Country Blues"
			     ": 0 4 20 I just"
			     "* 5 11 21  can't believe "
			     "E"))
    '(:artist "Joe Blow" :title "Country Blues" :notes ((:normal 0 4 20 "I just") (:golden 5 11 21 " can't believe "))))

(is-condition (parse-ultrastar-lines '("#ARTIST:Joe Blow"
				       "WTF? 0 4")) (make-condition 'invalid-song-file-line :text "WTF? 0 4"))

;(is (generate-ultrastar-string (parse-ultrastar-file "Pornophonique - Space Invaders.txt"))
;    (read-file "Pornophonique - Space Invaders.txt"))

(print-test-plan)