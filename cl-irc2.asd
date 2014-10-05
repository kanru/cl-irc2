(in-package #:cl-user)

(asdf:defsystem cl-irc2
  :version "0.0.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "MIT/Expat"
  :description "IRC Client Library"
  :components ((:module base
                :pathname "src"
                :components ((:file "packages")))
               (:module src
                :depends-on (base)
                :components ((:file "irc")
                             (:file "client"
                              :depends-on ("message" "connection"))
                             (:file "message")
                             (:file "connection")))))
