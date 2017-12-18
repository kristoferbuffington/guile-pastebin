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
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module ((system repl server) #:prefix repl:)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (fibers web server)
  #:use-module (web uri)
  #:use-module (pastebin render)
  #:use-module (pastebin template)
  #:use-module (pastebin paste)
  #:use-module (syntax-highlight)
  #:use-module (syntax-highlight scheme)
  #:use-module (wiredtiger wiredtiger)
  #:use-module (wiredtiger extra)
  #:use-module (wiredtiger feature-space)
  #:use-module (wiredtiger grf3)


  #:export (run-pastebin))

(define (decode-string bv charset)
  (if (string-ci=? charset "utf-8")
      (utf8->string bv)
      (let ((p (open-bytevector-input-port bv)))
        (set-port-encoding! p charset)
        (read-delimited "" p))))

(define* (parse-www-form-urlencoded str #:optional (charset "utf-8"))
  (map (lambda (piece)
         (let ((equals (string-index piece #\=)))
           (if equals
               (cons (uri-decode (substring piece 0 equals) #:encoding charset)
                     (uri-decode (substring piece (1+ equals)) #:encoding charset))
               (cons (uri-decode piece #:encoding charset) ""))))
       (string-split str #\&)))

(define (request-form-data request body)
  (if (bytevector? body)
      ;; Since valid application/x-www-form-urlencoded content only has
      ;; ascii characters, treat the incoming data as ascii (well,
      ;; latin-1), then use the charset when percent-decoding the
      ;; content.
      (request-form-data request (decode-string body "iso-8859-1"))
      (if (or (not body)
              (string-null? body))
          '()
          (let* ((content-type (request-content-type request))
                 (charset (or (assoc-ref (cdr content-type) "charset") "utf-8")))
            (cond
             ((equal? (car content-type) 'application/x-www-form-urlencoded)
              (parse-www-form-urlencoded body charset)))))))

(define (get-request? request)
  (eq? (request-method request) 'GET))

(define (post-request? request)
  (eq? (request-method request) 'POST))

(define (request-path-components request)
  "Split the URI path of REQUEST into a list of component strings.  For
example: \"/foo/bar\" yields '(\"foo\" \"bar\")."
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (make-request-handler)
  (lambda (request body)
    (format #t "~a ~a~%"
            (request-method request)
            (uri-path (request-uri request)))

    (cond
     ((get-request? request)
      (match (request-path-components request)
        ((or ("index.html") '())
         (render-html (template `(div (@ (id "content")
                                         (class "container-fluid"))
                                      (h1 "The index awww yeah")))))
        (("paste" "new")
         (render-html (template paste-form)))
        (("paste" uid)
         (let ((paste (get-paste uid)))
           (render-html (template `(div (@ (id "content")
                                           (class "container-fluid"))
                                        (h1 ,(paste-name paste))
                                        ;; TODO: Do not assume lex-scheme
                                        (pre (code ,(highlights->sxml
                                                     (highlight lex-scheme (paste-code paste))))))))))
        (("static" path ...)
         (render-static-asset request))
        (("favicon.ico")
         (render-static-asset request))
        (_
         (not-found request))))
     ((post-request? request)
      (match (request-path-components request)
        (("paste" "new")
         (if (bytevector? body)
             (let* ((data (request-form-data request body))
                    (name (cdr (car data)))
                    (code (cdr (car (cdr data))))
                    (uid (new-paste (make-paste name code))))
               (redirect (list "paste" uid)))
             (render-html (template (not-found request #:phrase "incorrect form data")))))
        (_
         (not-found request)))))))

(define* (run-pastebin #:key (repl? #f))
  (when repl?
    (repl:spawn-server (repl:make-tcp-server-socket)))

  (with-env paste-env
            (format #t "Server Started: http://localhost:8080/\n\n")
            (run-server (make-request-handler))))
