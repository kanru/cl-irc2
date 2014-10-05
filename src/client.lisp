;;;; client.lisp --- IRC Client interface

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

(defclass client ()
  ((%nick :initarg :nick
          :accessor client-nick)
   (%user :initarg :user
             :accessor client-user)
   (%realname :initarg :realname
              :accessor client-realname)
   (%channels :initarg :channels
              :accessor client-channels)))

(defgeneric send-message (message &rest args)
  (:method (message &rest args)
    (let ((msg (make-instance message :args args)))
      (send-raw-message (current-connection)
                        (irc-message:message-string msg)))))

(defgeneric recv-message ()
  (:method ()
    (let ((msg (recv-raw-message (current-connection))))
      (irc-message:make-message msg))))

(defgeneric handle-message (client message)
  (:method (client message)))

(defmethod handle-message (client (message irc-message:ping))
  (send-message 'irc-message:pong))

(defun comma-list (list &key key)
  (let ((list (if (atom list) (cl:list list) list)))
    (reduce (lambda (item1 item2)
                      (if item2 (format nil "~A,~A" item1 item2) item1))
                  list :key key)))

(defgeneric connect-run-main-loop (client)
  (:method (client)
    (send-message 'irc-message:nick (client-nick client))
    (send-message 'irc-message:user
                  (client-user client) "0" "*"
                  (client-realname client))
    (send-message 'irc-message:join (comma-list (client-channels client)))
    (loop (handle-message client (recv-message)))))

;;; client.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
