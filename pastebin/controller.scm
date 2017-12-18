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

(define-module (pastebin controller)
  #:use-module (ice-9 match)
  #:use-module (web util)
  #:use-module (web request)
  #:use-module (pastebin render)
  #:use-module (pastebin template)
  #:use-module (pastebin paste)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:export (controller-index
	    controller-get-paste
	    controller-new-paste-form
	    controller-new-paste))

(define (controller-index)
  (render-html (template `(div (@ (id "content")
				  (class "container-fluid"))
			       (div "This pastebin is under active development.")))))

(define (controller-get-paste uid)
  (let ((paste (get-paste uid)))
    (render-html (template `(div (@ (id "content")
				    (class "container-fluid"))
				 (h1 ,(paste-name paste))
				 ;; TODO: Do not assume lex-scheme
				 (pre (code ,(highlights->sxml
					      (highlight lex-scheme (paste-code paste))))))))))

(define (controller-new-paste-form)
  (render-html (template paste-form)))

(define (controller-new-paste request body)
  (match (request-form-data request body)
    ((("name" . name) ("code" . code))
     (redirect request
	       (list "paste"
		     (new-paste
		      (make-paste name code)))))
    (_
     ;; TODO: Use correct HTTP status code (415?)
     (not-found request #:phrase "incorrect form data"))))
