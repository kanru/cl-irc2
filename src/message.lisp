;;;; message.lisp --- IRC message handling

;;; Copyright (C) 2014  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; 

;;;; Code:

(in-package #:cl-irc2)

(defclass message ()
  ((%prefix :accessor message-prefix
            :initarg :prefix
            :initform nil)
   (%command :accessor message-command
             :initarg :command)
   (%args :accessor message-args
          :initarg :args)))

(defclass raw-message (message) ())

(defun format-message (prefix command &rest args)
  "Format a IRC message."
  (with-output-to-string (datum)
    (when prefix
      (format datum "~A " prefix))
    (format datum "~A" command)
    (maplist (lambda (subl)
               (format datum "~:[ ~; :~]~A" (endp (cdr subl)) (car subl)))
             (remove nil args))
    (format datum "~A~A" #\Return #\Linefeed)))

(defun message-string (message)
  "Convert a MESSAGE to a STRING suitable to send to remote."
  (with-accessors ((prefix message-prefix)
                   (command message-command)
                   (args message-args)) message
    (format-message prefix command args)))

(defun /pass (password)
  (format-message nil "PASS" password))

(defun /nick (nickname)
  (format-message nil "NICK" nickname))

(defclass nick-message (message) ())

(defun /user (user mode realname)
  (format-message nil "USER" user mode "*" realname))

(defun /oper (name password)
  (format-message nil "OPER" name password))

(defun /mode (nickname-or-channel &rest params)
  (apply #'format-message nil "MODE" nickname-or-channel params))

(defclass mode-mesage (message) ())

(defun /service (nickname distribution type info)
  (format-message nil "SERVICE" nickname "*" distribution type "*"  info))

(defun /quit (&optional quit-message)
  (format-message nil "QUIT" quit-message))

(defun /squit (server comment)
  (format-message nil "SQUIT" server comment))

(defun comma-list (list &key key)
  (let ((list (if (atom list) (list list) list)))
    (reduce (lambda (item1 item2)
                      (if item2 (format nil "~A,~A" item1 item2) item1))
                  list :key key)))

(defun /join (channel-key-list)
  (flet ((channel (item)
           (if (consp item) (car item) item))
         (key (item)
           (if (consp item) (cdr item) nil)))
    (let* ((channel-with-keys (remove-if #'stringp channel-key-list))
           (channel-only (remove-if #'consp channel-key-list))
           (channel-key-list (append channel-with-keys channel-only)))
      (format-message nil "JOIN"
                      (comma-list channel-key-list :key #'channel)
                      (comma-list channel-key-list :key #'key)))))

(defun /join0 ()
  (format-message nil "JOIN" "0"))

(defclass join-message (message) ())

(defun /part (channels &optional part-message)
  (format-message nil "PART" (comma-list channels) part-message))

(defclass part-message (message) ())

(defun /topic (channel &optional topic)
  (format-message nil "TOPIC" channel topic))

(defclass topic-message (message) ())

(defun /names (channels)
  (format-message nil "NAMES" (comma-list channels)))

(defun /list (channels)
  (format-message nil "LIST" (comma-list channels)))

(defun /invite (nickname channel)
  (format-message nil "INVITE" nickname channel))

(defclass invite-message (message) ())

(defun /kick (channels users &optional comment)
  (format-message nil "KICK" (comma-list channels) (comma-list users) comment))

(defun /privmsg (target text)
  (format-message nil "PRIVMSG" target text))

(defclass privmsg-message (message) ())

(defun /notice (target text)
  (format-message nil "NOTICE" target text))

(defclass notice-message (message) ())

(defun /motd (&optional target)
  (format-message nil "MOTD" target))

(defun /lusers (&optional mask target)
  (format-message nil "LUSERS" mask target))

(defun /version (&optional target)
  (format-message nil "VERSION" target))

(defun /stats (&optional query target)
  (format-message nil "STATS" query target))

(defun /links (&optional remote-server server-mask)
  (format-message nil "LINKS" remote-server server-mask))

(defun /time (&optional target)
  (format-message nil "TIME" target))

(defun /connect (target-server port &optional remote-server)
  (format-message nil "CONNECT" target-server port remote-server))

(defun /trace (&optional target)
  (format-message nil "TRACE" target))

(defun /admin (&optional target)
  (format-message nil "ADMIN" target))

(defun /info (&optional target)
  (format-message nil "INFO" target))

(defun /servlist (&optional mask type)
  (format-message nil "SERVLIST" mask type))

(defun /squery (servicename text)
  (format-message nil "SQUUERY" servicename text))

(defun /who (&optional mask oper)
  (format-message nil "WHO" mask (when oper "o")))

(defun /whois (users &optional target)
  (format-message nil "WHOIS" target (comma-list users)))

(defun /whowas (nicknames &optional count target)
  (format-message nil "WHOWAS" (comma-list nicknames) count target))

(defun /kill (nickname comment)
  (format-message nil "KILL" nickname comment))

(defun /ping (server1 &optional server2)
  (format-message nil "PING" server1 server2))

(defclass ping-message (message) ())

(defun /pong (server1 &optional server2)
  (format-message nil "PONG" server1 server2))

(defclass pong-message (message) ())

(defclass error-message (message) ())

(defun /away (&optional text)
  (format-message nil "AWAY" text))

(defclass away-message (message) ())

(defun /rehash ()
  (format-message nil "REHASH"))

(defun /die ()
  (format-message nil "DIE"))

(defun /restart ()
  (format-message nil "RESTART"))

(defun /summon (user &optional target channel)
  (format-message nil "SUMMON" user target channel))

(defun /users (&optional target)
  (format-message nil "TARGET" target))

(defun /wallops (text)
  (format-message nil "WALLOPS" text))

(defun /userhost (&rest nicknames)
  (apply #'format-message nil "USERHOST" nicknames))

(defun /ison (&rest nicknames)
  (apply #'format-message nil "ISON" nicknames))

(defun maybe-parse-prefix (string)
  (labels ((parse-prefix (prefix)
             (let ((@pos (position #\@ prefix)))
               (if @pos
                   (append (parse-nickname-user prefix @pos)
                           (list :host (subseq prefix (1+ @pos))))
                   prefix)))
           (parse-nickname-user (prefix @pos)
             (let ((!pos (position #\! prefix :end @pos)))
               (if !pos
                   (list :nickname (subseq prefix 0 !pos)
                         :user (subseq prefix (1+ !pos) @pos))
                   (list :nickname (subseq prefix 0 @pos))))))
    (if (char= #\: (char string 0))
        (let ((end (position #\Space string :start 1)))
          (values (parse-prefix (subseq string 1 end)) (1+ end)))
        (values nil 0))))

(defun parse-command (string cmd-start)
  (let ((end (position #\Space string :start cmd-start)))
    (values (subseq string cmd-start end) end)))

(defun parse-params (string params-start)
  (let ((point params-start)
        (n-param 0)
        (params (make-array 3 :fill-pointer 0 :adjustable t)))
    (labels ((ch ()
               (char string point))
             (nospcrlfcl-p (char)
               (not (member char '(#\Nul #\Return #\Newline #\Space #\:))))
             (middle-p (char)
               (or (char= #\: char)
                   (nospcrlfcl-p char)))
             (trailing-p (char)
               (or (char= #\: char)
                   (char= #\Space char)
                   (nospcrlfcl-p char)))
             (collect-trailing ()
               (let* ((start point)
                      (end (position-if-not #'trailing-p string :start start)))
                 (setf point end)
                 (vector-push-extend (subseq string start end) params)))
             (collect-middle ()
               (let* ((start point)
                      (end (position-if-not #'middle-p string :start start)))
                 (setf point end)
                 (vector-push-extend (subseq string start end) params))))
      (when (char= #\Space (ch))
        (loop :with found-trailing := nil
              :until (= n-param 15)
              :until found-trailing
              :until (not point)
              :when (char= #\Space (ch))
                :do (incf point)
                    (incf n-param)
                    (cond
                      ((char= #\: (ch))
                       (setf found-trailing t)
                       (incf point)
                       (collect-trailing))
                      (t
                       (collect-middle))))
        (coerce params 'list)))))

(defun parse-message (string)
  "Parse a RFC2812 message.

The Augmented BNF representation for a message is:

message    =  [ \":\" prefix SPACE ] command [ params ] crlf
prefix     =  servername / ( nickname [ [ \"!\" user ] \"@\" host ] )
command    =  1*letter / 3digit
params     =  *14( SPACE middle ) [ SPACE \":\" trailing ]
           =/ 14( SPACE middle ) [ SPACE [ \":\" ] trailing ]

nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
                ; any octet except NUL, CR, LF, \" \" and \":\"
middle     =  nospcrlfcl *( \":\" / nospcrlfcl )
trailing   =  *( \":\" / \" \" / nospcrlfcl )

SPACE      =  %x20        ; space character
crlf       =  %x0D %x0A   ; \"carriage return\" \"linefeed\""
  (multiple-value-bind (prefix cmd-start)
      (maybe-parse-prefix string)
    (multiple-value-bind (command params-start)
        (parse-command string cmd-start)
      (let ((params (parse-params string params-start)))
        (list prefix command params)))))

;;; message.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
