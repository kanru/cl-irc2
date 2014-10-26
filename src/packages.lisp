;;;; packages.lisp --- Packages

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

(defpackage #:cl-irc2-message
  (:use #:cl)
  (:nicknames #:irc-message)
  (:shadow #:restart
           #:time
           #:trace
           #:list)
  (:export #:message
           #:message-prefix
           #:message-command
           #:message-args
           #:message-string
           #:make-message
           #:pass
           #:nick
           #:user
           #:oper
           #:mode
           #:service
           #:quit
           #:squit
           #:join
           #:part
           #:invite
           #:privmsg
           #:notice
           #:squery
           #:kill
           #:rehash
           #:die
           #:restart
           #:wallops
           #:topic
           #:names
           #:list
           #:kick
           #:whois
           #:whowas
           #:userhost
           #:ison
           #:motd
           #:lusers
           #:version
           #:stats
           #:links
           #:time
           #:connect
           #:trace
           #:admin
           #:info
           #:servlist
           #:who
           #:ping
           #:pong
           #:away
           #:summon
           #:users
           #:rpl-message
           #:rpl-welcome
           #:rpl-yourhost
           #:rpl-created
           #:rpl-myinfo
           #:rpl-bounce
           #:rpl-userhost
           #:rpl-ison
           #:rpl-away
           #:rpl-unaway
           #:rpl-nowaway
           #:rpl-whoisuser
           #:rpl-whoisserver
           #:rpl-whoisoperator
           #:rpl-whoisidle
           #:rpl-endofwhois
           #:rpl-whoischannels
           #:rpl-whoiwasuser
           #:rpl-endofwhowas
           #:rpl-liststart
           #:rpl-list
           #:rpl-listend
           #:rpl-uniqopis
           #:rpl-channelmodeis
           #:rpl-notopic
           #:rpl-topic
           #:rpl-inviting
           #:rpl-summoning
           #:rpl-invitelist
           #:rpl-endofinvitelist
           #:rpl-exceptlist
           #:rpl-endofexceptlist
           #:rpl-version
           #:rpl-whoreply
           #:rpl-endofwho
           #:rpl-namereply
           #:rpl-endofnames
           #:rpl-links
           #:rpl-endoflinks
           #:rpl-banlist
           #:rpl-endofbanlist
           #:rpl-info
           #:rpl-endofinfo
           #:rpl-motdstart
           #:rpl-motd
           #:rpl-endofmotd
           #:rpl-youreoper
           #:rpl-rehashing
           #:rpl-youreservice
           #:rpl-time
           #:rpl-userstart
           #:rpl-users
           #:rpl-endofusers
           #:rpl-nousers
           #:rpl-tracelink
           #:rpl-traceconnecting
           #:rpl-tracehandshake
           #:rpl-traceunknown
           #:rpl-traceoperator
           #:rpl-traceuser
           #:rpl-traceserver
           #:rpl-traceservice
           #:rpl-tracenewtype
           #:rpl-traceclass
           #:rpl-tracereconnect
           #:rpl-tracelog
           #:rpl-traceend
           #:rpl-statslinkinfo
           #:rpl-statscommands
           #:rpl-endofstats
           #:rpl-statsuptime
           #:rpl-statsoline
           #:rpl-umodeis
           #:rpl-servlist
           #:rpl-servlistend
           #:rpl-luserclient
           #:rpl-luserop
           #:rpl-luserunknown
           #:rpl-luserchannels
           #:rpl-luserme
           #:rpl-adminme
           #:rpl-adminloc1
           #:rpl-adminloc2
           #:rpl-adminemail
           #:rpl-tryagain
           #:err-nosuchnick
           #:err-nosuchserver
           #:err-nosuchchannel
           #:err-cannotsendtochan
           #:err-toomanychannels
           #:err-wasnosuchnick
           #:err-toomanytargets
           #:err-nosuchservice
           #:err-noorigin
           #:err-norecipient
           #:err-notexttosend
           #:err-notoplevel
           #:err-wildtoplevel
           #:err-badmask
           #:err-unknowncommand
           #:err-nomotd
           #:err-noadmininfo
           #:err-fileerror
           #:err-nonicknamegiven
           #:err-erroneusnickname
           #:err-nicknameinuse
           #:err-nickcollision
           #:err-unavailresource
           #:err-usernotinchannel
           #:err-notonchannel
           #:err-useronchannel
           #:err-nologin
           #:err-summondisabled
           #:err-usersdisabled
           #:err-notregistered
           #:err-needmoreparams
           #:err-alreadyregistred
           #:err-nopermforhost
           #:err-passwdmismatch
           #:err-yourebannedcreep
           #:err-youwillbebanned
           #:err-keyset
           #:err-channelisfull
           #:err-unknownmode
           #:err-inviteonlychan
           #:err-bannedfromchan
           #:err-badchannelkey
           #:err-badchanmask
           #:err-nochanmodes
           #:err-banlistfull
           #:err-noprivileges
           #:err-chanoprivsneeded
           #:err-cantkillserver
           #:err-restricted
           #:err-uniqopprivsneeded
           #:err-nooperhost
           #:err-umodeunknownflag
           #:err-usersdontmatch
           #:rpl-serviceinfo
           #:rpl-endofservices
           #:rpl-service
           #:rpl-none
           #:rpl-whoischanop
           #:rpl-killdone
           #:rpl-closing
           #:rpl-closeend
           #:rpl-infostart
           #:rpl-myportis
           #:rpl-statscline
           #:rpl-statsnline
           #:rpl-statsiline
           #:rpl-statskline
           #:rpl-statsqline
           #:rpl-statsyline
           #:rpl-statsvline
           #:rpl-statslline
           #:rpl-statshline
           #:rpl-statssline
           #:rpl-statsping
           #:rpl-statsbline
           #:rpl-statsdline
           #:err-noservicehost))

(defpackage #:cl-irc2
  (:use #:cl)
  (:nicknames #:irc)
  (:export #:connection
           #:current-connection
           #:make-connection
           #:with-connection
           #:client
           #:client-user
           #:client-realname
           #:client-channels
           #:send-message
           #:handle-message
           #:connect-run-main-loop))

;;; packages.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
