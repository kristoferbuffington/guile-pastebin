;;; GNUPaste --- A Pastebin service for GNU
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
;;;
;;; This file is part of GNUPaste.
;;;
;;; GNUPaste is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNUPaste is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNUPaste.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnupaste render)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (htmlprag)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-1)
  #:use-module (gnupaste template)
  #:export (render-html
	    render-static-asset
	    not-found
	    redirect))

(define (redirect path)
  (let ((uri (build-uri 'http
                        #:host "localhost"
                        #:port 8080
                        #:path (string-append
                                "/" (encode-and-join-uri-path path)))))
    (values (build-response
	     #:code 301
	     #:headers `((content-type . (text/html))
			 (location . ,uri)))
	    (format #f "Redirect to ~a" (uri->string uri)))))

(define (render-html html)
  (values (build-response #:code 200
			  #:headers `((content-type . (text/html))))
	  (lambda (port)
	    (if html
		(begin
		  (display "<!DOCTYPE html>\n" port)
		  (write-shtml-as-html html port))))))

(define (directory? filename)
  (string=? filename (dirname filename)))

(define (file-extension file-name)
  (last (string-split file-name #\.)))

(define file-mime-types
  '(("css" . (text/css))
    ("js" . (text/javascript))
    ("png" . (image/png))
    ("jpg" . (image/jpg))
    ("ico" . (image/png))))

(define (render-static-asset request)
  (let ((file-name (string-append (getcwd)
				  (uri-path (request-uri request)))))
    (if (and (file-exists? file-name)
	     (not (directory? file-name)))
	(values `((content-type . ,(assoc-ref file-mime-types (file-extension file-name))))
		(call-with-input-file file-name get-bytevector-all))
	(not-found request))))

(define* (not-found request
                    #:key (phrase "Resource not found")
                    ttl)
  "Render 404 response for REQUEST."
  (values (build-response #:code 404
                          #:headers (if ttl
                                        `((content-type . (text/html))
					  (cache-control (max-age . ,ttl)))
                                        `((content-type . (text/html)))))
          (lambda (port)
	    (begin
	      (display "<!DOCTYPE html>\n" port)
	      (write-shtml-as-html
	       (template
		`(h1 ,(string-append phrase ": " (uri-path (request-uri request)))))
	       port)))))
