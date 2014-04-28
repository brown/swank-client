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

;;;; Swank client unit tests.

(in-package #:common-lisp-user)

(defpackage #:swank-client-test
  (:documentation "Test code in the SWANK-CLIENT package.")
  (:use #:common-lisp
        #:com.google.base
        #:hu.dwim.stefil
        #:swank-client)
  (:export #:test-swank-client))

(in-package #:swank-client-test)

(defsuite (test-swank-client :in root-suite) ()
  (run-child-tests))

(in-suite test-swank-client)

(defconst +server-count+ 4)
(defvar *server-port* 10000)

(defun unused-port ()
  #+google3 (port-picker:unused-port)
  #-google3 (incf *server-port*))

(defun create-swank-server ()
  (let ((port (unused-port)))
    (is port)
    (setf swank:*configure-emacs-indentation* nil)
    (swank:create-server :port port)))

(deftest simple-eval ()
  (with-slime-connection (connection "localhost" (create-swank-server))
    (is (= (slime-eval 123 connection) 123))))

(deftest simple-eval-async ()
  (with-slime-connection (connection "localhost" (create-swank-server))
    (let ((result nil)
          (result-lock (bordeaux-threads:make-lock "result lock")))
      (slime-eval-async 123
                        connection
                        (lambda (x)
                          (bordeaux-threads:with-lock-held (result-lock)
                            (setf result x))))
      (loop until (bordeaux-threads:with-lock-held (result-lock) result))
      (is (= result 123)))))

(deftest several-connections ()
  (let* ((server-ports (loop repeat +server-count+ collect (create-swank-server)))
         (connections (loop for port in server-ports collect (slime-connect "localhost" port)))
         (work (make-array +server-count+
                           :initial-contents (loop repeat +server-count+ for i from 2 collect i)))
         (golden (map 'vector (lambda (x) (* x 2)) work)))
    (unwind-protect
         (let ((results (make-array +server-count+ :initial-element nil))
               (results-lock (bordeaux-threads:make-lock "results lock")))
           ;; Synchronous
           (loop for i below (length work)
                 for connection in connections
                 do (setf (aref results i) (slime-eval `(* 2 ,(aref work i)) connection)))
           (is (equalp results golden))
           ;; Reset results.
           (loop for i below (length results) do (setf (aref results i) nil))
           ;; Asynchronous
           (loop for i below (length work)
                 for connection in connections
                 do (let ((index i))
                      (slime-eval-async `(* 2 ,(aref work i))
                                        connection
                                        (lambda (result)
                                          (bordeaux-threads:with-lock-held (results-lock)
                                            (setf (aref results index) result))))))
           (loop while (bordeaux-threads:with-lock-held (results-lock) (some #'null results)))
           (is (equalp results golden)))
      (dolist (connection connections)
        (slime-close connection)))))

(deftest non-ascii-characters ()
  (flet ((create-string (code)
           (concatenate 'string "hello " (string (code-char code)) " world")))
      (with-slime-connection (connection "localhost" (create-swank-server))
        (loop for code from 0 below 2000 by 100 do
          (let ((string (create-string code)))
            (is (string= (slime-eval string connection) string)))))))
