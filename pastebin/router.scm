;;; Pastebin --- A Pastebin service for GNU
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
;;; Copyright © 2017 Jelle Licht <wordempire@gmail.com>
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

(define-module (pastebin router)
  #:use-module (ice-9 match)
  #:use-module (web util)
  #:use-module (web request)
  #:use-module (web uri)

  #:use-module (wiredtiger extra)

  #:use-module (pastebin controller)
  #:use-module (pastebin render)

  #:export (route))

(define (route)
  (lambda (request body)
    (with-context*
        (format #t "~a ~a~%"
                (request-method request)
                (uri-path (request-uri request)))

      (cond
       ((get-request? request)
        (match (request-path-components request)
          ((or ("index.html") '())
           (controller-index))
          (("paste" "new")
           (controller-new-paste-form))
          (("paste" uid)
           (controller-get-paste uid))
          (("static" path ...)
           (render-static-asset request))
          (("favicon.ico")
           (render-static-asset request))
          (_
           (not-found request))))
       ((post-request? request)
        (match (request-path-components request)
          (("paste" "new")
           (controller-new-paste request body))
          (_
           (not-found request #:phrase "incorrect form data"))))))))
