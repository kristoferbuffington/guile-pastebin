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

(define-module (pastebin template)
  #:use-module (htmlprag)
  #:use-module (twitter bootstrap)
  #:export (template
	    paste-form))

(define link-pastebin-css
  `(link
    (@ (rel "stylesheet")
       (href
	"/static/css/pastebin.css"))))

(define paste-form
  `(form (@ (class "container-fluid")
	    (action "/paste/new")
	    (method "post"))
	 (div (@ (class "form-group"))
	      (label (@ (for "name"))
		     "Name")
	      (input (@ (class "form-control")
			(id "name")
			(name "name")
			(type "text"))))
	 (div (@ (class "form-group"))
	      (label (@ (for "code"))
		     "Code")
	      (textarea (@ (class "form-control")
			   (id "code")
			   (rows "3")
			   (name "code"))))
	 (button (@ (class "btn btn-primary")
		    (type "submit"))
		 "Paste!")))

(define navigation
  `(nav (@ (class "navbar sticky-top navbar-toggleable-sm navbar-light bg-faded")
	  (id "navigation"))
       (button (@ (class "navbar-toggler navbar-toggler-right")
		  (type "button")
		  (data-toggle"collapse")
		  (data-target "#navbarSupportedContent")
		  (aria-controls "navbarSupportedContent")
		  (aria-expanded "false")
		  (aria-label "Toggle navigation"))
	       (span (@ (class "navbar-toggler-icon"))))
       (a (@ (class "navbar-brand")
	     (href "/"))
	  "Pastebin")
       (div (@ (class "collapse navbar-collapse")
	       (id "navbarSupportedContent"))
	    (ul (@ (class "navbar-nav mr-auto"))
		(li (@ (class "nav-item"))
		    (a (@ (class "nav-link")
			  (href "/paste/new"))
		       "New"))))))

(define footer
  `(div (@ (class "container-fluid")
	   (id "footer"))
	"Pastebin - " (a (@ (href "https://github.com/kristoferbuffington/guile-pastebin/"))
			 "source code")))
(define (template content)
  `(html
    (@ (lang "en"))
    (head
     (title "Pastebin")
     (meta (@ (charset "utf-8")))
     (meta
      (@ (name "viewport")
	 (content "width=device-width, initial-scale=1, shrink-to-fit=no")))
     ,link-bootstrap-css
     ,link-pastebin-css)
    (body (@ (data-spy "scroll"))
	  ,navigation
	  ,content
	  ,footer)))
