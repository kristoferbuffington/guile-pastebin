;;; Pastebin --- A Pastebin service for GNU
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
;;;
;;; This file is part of Pastebin.
;;;
;;; Pastebin is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Pastebin is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Pastebin.  If not, see <http://www.gnu.org/licenses/>.

(define-module (pastebin)
  #:use-module ((system repl server) #:prefix repl:)
  #:use-module (fibers web server)
  #:use-module (wiredtiger extra)
  #:use-module (wiredtiger feature-space)
  #:use-module (pastebin router)

  #:export (run-pastebin))

(define %wt-env
  (env-open* (string-append (getcwd) "/wt")
	     (list *feature-space*)
	     "create"))

(define* (run-pastebin #:key (repl? #f))
  (when repl?
    (repl:spawn-server (repl:make-tcp-server-socket)))

  (with-env %wt-env
            (format #t "Server Started: http://localhost:8080/\n\n")
            (run-server (route))))
