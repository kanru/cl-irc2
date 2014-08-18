;;;; test.lisp --- Testing

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

(in-package #:cl-user)

(defpackage #:cl-irc2-test
  (:use #:cl #:clunit))

(in-package #:cl-irc2-test)

(defsuite message-parser ())

(deftest parse-prefix (message-parser)
  (assert-equal "prefix" (cl-irc2::maybe-parse-prefix ":prefix "))
  (assert-equal nil (cl-irc2::maybe-parse-prefix "not-prefix"))
  (assert-equal
      "tolsun.oulu.fi"
      (cl-irc2::maybe-parse-prefix ":tolsun.oulu.fi "))
  (assert-equal
      '(:nickname "WiZ" :host "tolsun.oulu.fi")
      (cl-irc2::maybe-parse-prefix ":WiZ@tolsun.oulu.fi "))
  (assert-equal
      '(:nickname "WiZ" :user "jto" :host "tolsun.oulu.fi")
      (cl-irc2::maybe-parse-prefix ":WiZ!jto@tolsun.oulu.fi ")))

(deftest parse-command (message-parser)
  (assert-equal "PASS" (cl-irc2::parse-command ":p PASS " 3))
  (assert-equal "001" (cl-irc2::parse-command ":p 001 " 3)))

(deftest parse-params (message-parser)
  (assert-equal
      '("trailing")
      (cl-irc2::parse-params " :trailing" 0))
  (assert-equal
      '("param1" "trailing")
      (cl-irc2::parse-params " param1 :trailing" 0))
  (assert-equal
      '("param1" "param2" "trailing")
      (cl-irc2::parse-params " param1 param2 :trailing" 0))
  (assert-equal
      '("param1" "param2" "trailing trailing")
      (cl-irc2::parse-params " param1 param2 :trailing trailing" 0))
  (assert-equal
      '("param1" "param2" "param3")
      (cl-irc2::parse-params " param1 param2 param3" 0))
  (assert-equal
      '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15")
      (cl-irc2::parse-params " 1 2 3 4 5 6 7 8 9 10 11 12 13 14 :15" 0))
  (assert-equal
      '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15")
      (cl-irc2::parse-params " 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15" 0)))

(deftest parse-message (message-parser)
  (assert-equal
      '((:nickname "WiZ" :user "jto" :host "tolsun.oulu.fi") "NICK" ("Kilroy"))
      (cl-irc2::parse-message ":WiZ!jto@tolsun.oulu.fi NICK Kilroy"))
  (assert-equal
      '((:nickname "WiZ" :user "jto" :host "tolsun.oulu.fi") "NICK" ("Kilroy"))
      (cl-irc2::parse-message ":WiZ!jto@tolsun.oulu.fi NICK :Kilroy"))
  (assert-equal
      '(nil "PASS" ("secretpasswordhere"))
      (cl-irc2::parse-message "PASS secretpasswordhere"))
  (assert-equal
      '(nil "USER" ("guest" "0" "*" "Ronnie Reagan"))
      (cl-irc2::parse-message "USER guest 0 * :Ronnie Reagan"))
  (assert-equal
      '(nil "QUIT" ("Gone to have lunch"))
      (cl-irc2::parse-message "QUIT :Gone to have lunch"))
  (assert-equal
      '((:nickname "syrk" :user "kalt" :host "millennium.stealth.net")
        "QUIT" ("Gone to have lunch"))
      (cl-irc2::parse-message
       ":syrk!kalt@millennium.stealth.net QUIT :Gone to have lunch"))
  (assert-equal
      '("Trillian" "SQUIT" ("cm22.eng.umd.edu" "Server out of control"))
      (cl-irc2::parse-message
       ":Trillian SQUIT cm22.eng.umd.edu :Server out of control")))

;;; test.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
