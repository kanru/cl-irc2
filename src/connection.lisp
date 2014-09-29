;;;; connection.lisp --- IRC client library

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

(defvar *current-connection*)

(defclass connection ()
  ((%stream :initarg :stream
            :accessor connection-stream)))

(defun current-connection ()
  *current-connection*)

(defun make-connection (stream)
  (make-instance 'connection :stream stream))

(defmacro with-connection ((connection) &body body)
  `(let ((*current-connection* ,connection))
     ,@body))

(defgeneric send-raw-message (connection msg)
  (:method (connection msg)
    (check-type msg string)
    (let ((stream (connection-stream connection)))
      (write-line msg stream)
      (force-output stream))))

(defgeneric recv-raw-message (connection)
  (:method (connection)
    (let ((stream (connection-stream connection)))
      (read-line stream))))

;;; connection.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
