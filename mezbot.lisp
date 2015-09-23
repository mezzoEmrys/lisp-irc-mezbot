(require :cl-irc)
(require :cl-ppcre)
(defpackage :mezbot
	(:use :cl :irc :cl-ppcre))
(in-package :mezbot)
(load "secrets.lisp")
(defvar *chat-channel* "#extratricky")
(defvar *connection* (connect :username "mezzoemrysbot"
							:nickname "mezzoemrysbot"
							:server "irc.twitch.tv"
							:port 6667
							:connection-security :none
							:password *mezbot-password*))
(join *connection* *chat-channel*)

(defun get-irc-message (message)
	(car (last (arguments message))))
(defun get-irc-sender (message)
	(source message))
(defun get-irc-location (message)
	(first (arguments message)))
(defun message-skip-command (message)
	(join-list-string (rest (split-by-one-space (get-irc-message message)))))

(defun message-equal (message compare)
	(string-equal (get-irc-message message) compare))
(defun message-contain (message compare)
	(search compare (get-irc-message message)))
(defun message-begins (message compare)
	(scan (concatenate 'string "^" compare) (get-irc-message message)))
(defun message-match (message regex)
	(scan regex (get-irc-message message)))
(defun message-sender (message sender)
	(equal sender (get-irc-sender message)))

(defun send-message (message &optional (destination *chat-channel*))
	(privmsg *connection* destination message))

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(defun join-list-string (string-list)
	(format nil "~{~A~^ ~}" string-list))

(defun append-to-list (base-list new-list)
	(setf base-list (append base-list new-list)))

(defvar *hanabi-play-list* '("newest" "oldest" "with the finesse" "blind" "most clued" "around the bluff"))
(defvar *hanabi-clue-list* '("purple" "red" "blue" "yellow" "green" "1" "2" "3" "4" "5"))
(defvar *hanabi-discard-list* '("chop" "chop" "newest card" "oldest card" "nothing!" "second newest" "second oldest" "third oldest"))
(defvar *hanabi-players-list* '(""))

(defmacro pick-random (random-list)
	`(nth (random (length ,random-list)) ,random-list))

(defun msg-hook (message)
	(let ((destination (get-irc-location message)))
		(progn 
			(cond 
				((message-contain message "twingePrivilege")
					(send-message "Who turned off the oxygen?" destination))
				((message-equal message "!hanabi-play")
					(send-message (concatenate 'string "play " (pick-random *hanabi-play-list*)) destination))
				((message-equal message "!hanabi-clue")
					(send-message (concatenate 'string "clue " (pick-random *hanabi-players-list*) " " (pick-random *hanabi-clue-list*)) destination))
				((message-equal message "!hanabi-discard")
					(send-message (concatenate 'string "discard " (pick-random *hanabi-discard-list*)) destination))
				((message-equal message "!hanabi-players")
					(send-message (concatenate 'string "Current (non-streamer) players: " (join-list-string *hanabi-players-list*)) destination))
				((message-equal message "!hanabi-move")
					(let ((move (pick-random '("play" "play" "play" "play" "play" "clue" "clue" "clue" "clue" "discard" "discard"))))
						(cond
							((string-equal move "play")
								(send-message (concatenate 'string "play " (pick-random *hanabi-play-list*)) destination))
							((string-equal move "clue")
								(send-message (concatenate 'string "clue " (pick-random *hanabi-players-list*) " " (pick-random *hanabi-clue-list*)) destination))
							((string-equal move "discard")
								(send-message (concatenate 'string "discard " (pick-random *hanabi-discard-list*)) destination)))))
				((message-equal message "!hanabi-set-players")
					(progn
						(setf *hanabi-players-list* '(""))
						(send-message "Players list emptied!") destination))
				((message-begins message "!hanabi-set-players")
					(progn
						(setf *hanabi-players-list* (rest (split-by-one-space (get-irc-message message))))
						(send-message "Players list updated!")) destination)
				((and (message-contain message "unarmed combat") (or (message-contain message "mezzoemrysbot") (message-contain message "mezbot")))
					(send-message "Watch out, I'm trained in google-fu!" destination))
				((message-equal message "!hearthstone-draft")
					(send-message (pick-random '("pick left" "pick right" "pick middle")) destination))
				((message-equal message "!boom")
					(send-message "Woah! Watch the explosives!" destination))
				((message-contain message "go face")
					(send-message "Yup, face is the place, definitely." destination))
				((and (message-sender message "mezzoemrys") (message-begins message "!eval"))
					(send-message (eval (read-from-string (message-skip-command message))) destination))
				((and (message-sender message "mezzoemerys") (message-begins message "!silent-eval"))
					(eval (read-from-string (message-skip-command message))))
				((and (message-sender message "twitchnotify") (message-match "[^ ]*? has just subscribed"))
					(send-message "Oh boy, another subscriber!"))
				(t nil))
			(print (concatenate 'string (source message) ": " (car (rest (arguments message))))))))

(defun sal-hook (message)
	(let ((destination *chat-channel*))
		(send-message (pick-random '("Mezbot has joined the party!" "It's Mezbot time!" "Hey guys, what's happening?")))))
			
(add-hook *connection* 'irc-privmsg-message 'msg-hook)
;(add-hook *connection* 'irc-join-message 'sal-hook)
(read-message-loop *connection*)


