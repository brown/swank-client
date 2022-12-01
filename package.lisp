;;;; Copyright 2011 Google Inc.

;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;;; MA  02110-1301, USA.

;;;; Author: Robert Brown <robert.brown@gmail.com>

(in-package #:common-lisp-user)

(defpackage #:swank-client
  (:documentation "A client interface to Swank servers.")
  (:use #:common-lisp)
  (:import-from #:com.google.base
                #:defconst
                #:make-octet-vector
                #:missing-argument
                #:octet
                #:string-to-utf8-octets
                #:utf8-octets-to-string)
  (:import-from #:bordeaux-threads
                #:condition-notify
                #:condition-wait
                #:make-condition-variable
                #:make-lock
                #:make-thread
                #:with-lock-held)
  (:import-from #:usocket
                #:socket
                #:socket-close
                #:socket-connect
                #:socket-error
                #:socket-stream
                #:stream-usocket)
  (:export #:swank-connection
           #:slime-connect
           #:slime-close
           #:slime-eval
           #:slime-eval-async
           #:slime-migrate-evals
           #:slime-network-error
           #:slime-pending-evals-p
           #:with-slime-connection))
