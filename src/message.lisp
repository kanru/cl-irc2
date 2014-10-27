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

(in-package #:cl-irc2-message)

(defvar *message-class-hashtable* (make-hash-table :test #'equal))

(defclass message ()
  ((%prefix :accessor message-prefix
            :initarg :prefix
            :initform nil)
   (%command :accessor message-command
             :initarg :command)
   (%args :accessor message-args
          :initarg :args)))

(defmacro defmessage (class &rest args)
  (declare (ignore args))
  (let ((command (string-upcase (symbol-name class))))
    `(progn
       (defclass ,class (message)
         ((%command :initform ,command)))
       (setf (gethash ,command *message-class-hashtable*) ',class))))

(defun list (&rest args)
  (declare (inline))
  (apply #'cl:list args))

(defmessage pass (password))
(defmessage nick (nickname))
(defmessage user (user mode realname))
(defmessage oper (name password))
(defmessage mode (nickname-or-channel &rest params))
(defmessage service (nickname distribution type info))
(defmessage quit (&optional quit-message))
(defmessage squit (server comment))
(defmessage join (channel-key-list))
(defmessage part (&optional message))
(defmessage invite (nickname channel))
(defmessage privmsg (target text))
(defmessage notice (target text))
(defmessage squery (servicename text))
(defmessage kill (nickname comment))
(defmessage rehash)
(defmessage die)
(defmessage restart)
(defmessage wallops (text))
(defmessage topic (channel &optional topic))
(defmessage names (channels))
(defmessage list (channels))
(defmessage kick (channels users &optional comment))
(defmessage whois (users &optional target))
(defmessage whowas (nicknames &optional count target))
(defmessage userhost (&rest nicknames))
(defmessage ison (&rest nicknames))
(defmessage motd (&optional target))
(defmessage lusers (&optional mask target))
(defmessage version (&optional target))
(defmessage stats (&optional query target))
(defmessage links (&optional remote-server server-mask))
(defmessage time (&optional target))
(defmessage connect (target-server port &optional remote-server))
(defmessage trace (&optional target))
(defmessage admin (&optional target))
(defmessage info (&optional target))
(defmessage servlist (&optional mask type))
(defmessage who (&optional mask oper))
(defmessage ping (server1 &optional server2))
(defmessage pong (server1 &optional server2))
(defmessage away (&optional text))
(defmessage summon (user &optional target channel))
(defmessage users (&optional target))

(defclass rpl-message (message) ())

(defmacro defrpl (number class &rest args)
  (declare (ignore args))
  (let ((command (format nil "~3,'0d" number)))
    `(progn
       (defclass ,class (rpl-message) ())
       (setf (gethash ,command *message-class-hashtable*) ',class))))

(defrpl 001 rpl-welcome)
(defrpl 002 rpl-yourhost)
(defrpl 003 rpl-created)
(defrpl 004 rpl-myinfo)
(defrpl 005 rpl-bounce)
(defrpl 302 rpl-userhost)
(defrpl 303 rpl-ison)
(defrpl 301 rpl-away)
(defrpl 305 rpl-unaway)
(defrpl 306 rpl-nowaway)
(defrpl 311 rpl-whoisuser)
(defrpl 312 rpl-whoisserver)
(defrpl 313 rpl-whoisoperator)
(defrpl 317 rpl-whoisidle)
(defrpl 318 rpl-endofwhois)
(defrpl 319 rpl-whoischannels)
(defrpl 314 rpl-whoiwasuser)
(defrpl 369 rpl-endofwhowas)
(defrpl 321 rpl-liststart)
(defrpl 322 rpl-list)
(defrpl 323 rpl-listend)
(defrpl 325 rpl-uniqopis)
(defrpl 324 rpl-channelmodeis)
(defrpl 331 rpl-notopic)
(defrpl 332 rpl-topic)
(defrpl 341 rpl-inviting)
(defrpl 342 rpl-summoning)
(defrpl 346 rpl-invitelist)
(defrpl 347 rpl-endofinvitelist)
(defrpl 348 rpl-exceptlist)
(defrpl 349 rpl-endofexceptlist)
(defrpl 351 rpl-version)
(defrpl 352 rpl-whoreply)
(defrpl 315 rpl-endofwho)
(defrpl 353 rpl-namereply)
(defrpl 366 rpl-endofnames)
(defrpl 364 rpl-links)
(defrpl 365 rpl-endoflinks)
(defrpl 367 rpl-banlist)
(defrpl 368 rpl-endofbanlist)
(defrpl 371 rpl-info)
(defrpl 374 rpl-endofinfo)
(defrpl 375 rpl-motdstart)
(defrpl 372 rpl-motd)
(defrpl 376 rpl-endofmotd)
(defrpl 381 rpl-youreoper)
(defrpl 382 rpl-rehashing)
(defrpl 383 rpl-youreservice)
(defrpl 391 rpl-time)
(defrpl 392 rpl-userstart)
(defrpl 393 rpl-users)
(defrpl 394 rpl-endofusers)
(defrpl 395 rpl-nousers)
(defrpl 200 rpl-tracelink)
(defrpl 201 rpl-traceconnecting)
(defrpl 202 rpl-tracehandshake)
(defrpl 203 rpl-traceunknown)
(defrpl 204 rpl-traceoperator)
(defrpl 205 rpl-traceuser)
(defrpl 206 rpl-traceserver)
(defrpl 207 rpl-traceservice)
(defrpl 208 rpl-tracenewtype)
(defrpl 209 rpl-traceclass)
(defrpl 210 rpl-tracereconnect)
(defrpl 261 rpl-tracelog)
(defrpl 262 rpl-traceend)
(defrpl 211 rpl-statslinkinfo)
(defrpl 212 rpl-statscommands)
(defrpl 219 rpl-endofstats)
(defrpl 242 rpl-statsuptime)
(defrpl 243 rpl-statsoline)
(defrpl 221 rpl-umodeis)
(defrpl 234 rpl-servlist)
(defrpl 235 rpl-servlistend)
(defrpl 251 rpl-luserclient)
(defrpl 252 rpl-luserop)
(defrpl 253 rpl-luserunknown)
(defrpl 254 rpl-luserchannels)
(defrpl 255 rpl-luserme)
(defrpl 256 rpl-adminme)
(defrpl 257 rpl-adminloc1)
(defrpl 258 rpl-adminloc2)
(defrpl 259 rpl-adminemail)
(defrpl 263 rpl-tryagain)
(defrpl 401 err-nosuchnick)
(defrpl 402 err-nosuchserver)
(defrpl 403 err-nosuchchannel)
(defrpl 404 err-cannotsendtochan)
(defrpl 405 err-toomanychannels)
(defrpl 406 err-wasnosuchnick)
(defrpl 407 err-toomanytargets)
(defrpl 408 err-nosuchservice)
(defrpl 409 err-noorigin)
(defrpl 411 err-norecipient)
(defrpl 412 err-notexttosend)
(defrpl 413 err-notoplevel)
(defrpl 414 err-wildtoplevel)
(defrpl 415 err-badmask)
(defrpl 421 err-unknowncommand)
(defrpl 422 err-nomotd)
(defrpl 423 err-noadmininfo)
(defrpl 424 err-fileerror)
(defrpl 431 err-nonicknamegiven)
(defrpl 432 err-erroneusnickname)
(defrpl 433 err-nicknameinuse)
(defrpl 436 err-nickcollision)
(defrpl 437 err-unavailresource)
(defrpl 441 err-usernotinchannel)
(defrpl 442 err-notonchannel)
(defrpl 443 err-useronchannel)
(defrpl 444 err-nologin)
(defrpl 445 err-summondisabled)
(defrpl 446 err-usersdisabled)
(defrpl 451 err-notregistered)
(defrpl 461 err-needmoreparams)
(defrpl 462 err-alreadyregistred)
(defrpl 463 err-nopermforhost)
(defrpl 464 err-passwdmismatch)
(defrpl 465 err-yourebannedcreep)
(defrpl 466 err-youwillbebanned)
(defrpl 467 err-keyset)
(defrpl 471 err-channelisfull)
(defrpl 472 err-unknownmode)
(defrpl 473 err-inviteonlychan)
(defrpl 474 err-bannedfromchan)
(defrpl 475 err-badchannelkey)
(defrpl 476 err-badchanmask)
(defrpl 477 err-nochanmodes)
(defrpl 478 err-banlistfull)
(defrpl 481 err-noprivileges)
(defrpl 482 err-chanoprivsneeded)
(defrpl 483 err-cantkillserver)
(defrpl 484 err-restricted)
(defrpl 485 err-uniqopprivsneeded)
(defrpl 491 err-nooperhost)
(defrpl 501 err-umodeunknownflag)
(defrpl 502 err-usersdontmatch)
(defrpl 231 rpl-serviceinfo)
(defrpl 232 rpl-endofservices)
(defrpl 233 rpl-service)
(defrpl 300 rpl-none)
(defrpl 316 rpl-whoischanop)
(defrpl 361 rpl-killdone)
(defrpl 362 rpl-closing)
(defrpl 363 rpl-closeend)
(defrpl 373 rpl-infostart)
(defrpl 384 rpl-myportis)
(defrpl 213 rpl-statscline)
(defrpl 214 rpl-statsnline)
(defrpl 215 rpl-statsiline)
(defrpl 216 rpl-statskline)
(defrpl 217 rpl-statsqline)
(defrpl 218 rpl-statsyline)
(defrpl 240 rpl-statsvline)
(defrpl 241 rpl-statslline)
(defrpl 244 rpl-statshline)
(defrpl 244 rpl-statssline)
(defrpl 246 rpl-statsping)
(defrpl 247 rpl-statsbline)
(defrpl 250 rpl-statsdline)
(defrpl 492 err-noservicehost)

(defun format-message (prefix command args)
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
               (not (member char '(#\Nul #\Return #\Linefeed #\Space #\:))))
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
              :until (member (ch) '(#\Return #\Linefeed))
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
        (coerce params 'cl:list)))))

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

(defun make-message (raw-message)
  (let* ((msg (parse-message raw-message))
         (prefix (car msg))
         (command (cadr msg))
         (args (caddr msg))
         (class (gethash command *message-class-hashtable* 'message)))
    (make-instance class :prefix prefix
                         :command command
                         :args args)))

;;; message.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
