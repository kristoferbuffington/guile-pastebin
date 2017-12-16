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

(define-module (pastebin paste)
  #:use-module (wiredtiger wiredtiger)
  #:use-module (wiredtiger extra)
  #:use-module (wiredtiger feature-space)
  #:use-module (wiredtiger grf3)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 rdelim)
  #:use-module (microkanren)
  #:use-module (pastebin config)
  
  :export (new-paste
	   get-paste
	   make-paste
	   paste-name
	   paste-code
	   paste-env))

(define paste-env
  (env-open* (string-append (getcwd) "/wt")
	     (list *feature-space*)
	     "create"))

(define (temp-env)
  (env-open* (string-append (getcwd) "/wt")
	     (list *feature-space*)
	     "create"))

(define-record-type <paste>
  (make-paste name code)
  paste?
  (name paste-name)
  (code paste-code))

(define (uuid)
    (with-input-from-file
	"/proc/sys/kernel/random/uuid"
      (lambda ()
	(read-line (current-input-port)))))

(define (new-paste paste)
  (if (paste? paste)
      (fs:add! `((kind . paste)
		 (paste/name . ,(paste-name paste))
		 (paste/code . ,(paste-code paste))))
	#f))

(define (get-paste uid)
  (let* ((data (fs:ref* uid))
	 (name (cdar data))
	 (code (cdar (cdr data))))
    (make-paste name code)))

(define (list-pastes-query)
  (reverse (sort! (run* (paste/uid)
		    (fresh (uid?)
		      (fs:queryo uid? `kind `paste)))
		  (lambda (a b) (< (list-ref a 1) (list-ref b 1))))))
