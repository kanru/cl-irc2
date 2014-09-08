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

;;; irc.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
