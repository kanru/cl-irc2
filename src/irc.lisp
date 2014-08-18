;;;; irc.lisp --- IRC client library

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

;;; See RFC 2812

;;;; Code:

(in-package #:cl-irc2)

(defclass server ()
  ((%name :initarg :name
          :accessor server-name
          :documentation
          "Servers are uniquely identified by their name, which has a
maximum length of sixty three (63) characters.")))

(defclass client ()
  ())

(defclass user ()
  ((%nick :initarg :nick
          :accessor user-nick
          :documentation
          "Each user is distinguished from other users by a unique
nickname having a maximum length of nine (9) characters.")
   (%op-p :initarg :operator
          :accessor user-operator-p
          :documentation
          "A special class of users (operators) is allowed to perform
general maintenance functions on the network.")))

(defclass service ()
  ((%nick :initarg :nick
          :accessor service-nick)
   (%server :initarg :server
            :accessor service-server-name)))

(defclass channel ()
  ((%name :initarg :name
          :accessor channel-name
          :documentation
          "Channels names are strings (beginning with a '&', '#', '+'
or '!' character) of length up to fifty (50) characters. Apart from
the requirement that the first character is either '&', '#', '+' or
'!', the only restriction on a channel name is that it SHALL NOT
contain any spaces (' '), a control G (^G or ASCII 7), a comma (',').
Space is used as parameter separator and command is used as a list
item separator by the protocol). A colon (':') can also be used as a
delimiter for the channel mask. Channel names are case
insensitive.")))

(defparameter *normalized-characters-alist* '((#\{ . #\[)
                                              (#\} . #\])
                                              (#\| . #\\)
                                              (#\^ . #\~))
  "Because of IRC's Scandinavian origin, the characters {}|^ are
considered to be the lower case equivalents of the characters []\~,
respectively. This is a critical issue when determining the
equivalence of two nicknames or channel names.")

(defclass message ()
  ((%prefix :initarg :prefix
            :initform nil
            :accessor message-prefix)
   (%command :initarg :command
             :accessor message-command)
   (%args :initarg :args
          :accessor message-args)))

(defclass command (message)
  ())

(defgeneric message-string (message)
  (:documentation "Convert a MESSAGE to a STRING suitable to send to remote."))

(defmethod message-string ((msg message))
  (format nil "~@[:~A ~]~A~{ ~A~}~A~A"
          (message-prefix msg)
          (message-command msg)
          (message-args msg)
          #\Return #\Linefeed))

(defun /pass (password)
  (make-instance 'message :command :pass :args (list password)))

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

;;; irc.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
