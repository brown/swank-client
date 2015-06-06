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

(defsystem swank-client-test
  :name "Swank Client test"
  :description "Test code for package SWANK-CLIENT."
  :version "1.5"
  :author "Robert Brown"
  :license "GPL version 2.  See the copyright messages in individual files."
  :depends-on (bordeaux-threads
               hu.dwim.stefil
               swank
               swank-client)
  :components
  ((:file "swank-client_test")))

(defmethod perform ((operation test-op) (component (eql (find-system 'swank-client-test))))
  (funcall (read-from-string "swank-client-test:test-swank-client")))
