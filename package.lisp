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

;;;; Author: brown@google.com (Robert Brown)

(in-package #:common-lisp-user)

(defpackage #:swank-printing
  (:documentation
   "A package that imports no symbols, used by the Swank client code when
printing s-expression so that symbols in the printed output contain their
package names.")
  (:use))

(defpackage #:swank-client
  (:documentation "A client interface to Swank servers.")
  (:use #:common-lisp #:com.google.base)
  (:export #:swank-connection
           #:slime-connect
           #:slime-close
           #:slime-eval
           #:slime-eval-async
           #:slime-migrate-evals
           #:slime-network-error
           #:slime-pending-evals-p
           #:with-slime-connection))
