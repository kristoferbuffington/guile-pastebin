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

(define-module (twitter bootstrap)
  #:use-module (htmlprag)

  #:export (link-bootstrap-css
	    script-bootstrap-jquery
	    script-tether
	    script-bootstrap))

(define link-bootstrap-css
  `(link
    (@ (rel "stylesheet")
       (href
	"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css")
       (integrity
	"sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ")
       (crossorigin "anonymous"))))

(define script-bootstrap-jquery
  `(script
    (@ (src "https://code.jquery.com/jquery-3.1.1.slim.min.js")
       (integrity
	"sha384-A7FZj7v+d/sdmMqp/nOQwliLvUsJfDHW+k9Omg/a/EheAdgtzNs3hpfag6Ed950n")
       (crossorigin "anonymous"))))

(define script-tether
  `(script
    (@ (src
	"https://cdnjs.cloudflare.com/ajax/libs/tether/1.4.0/js/tether.min.js")
       (integrity
	"sha384-DztdAPBWPRXSA/3eYEEUWrWCy7G5KFbe8fFjk5JAIxUYHKkDx6Qin1DkWx51bBrb")
       (crossorigin "anonymous"))))

(define script-bootstrap
  `(script
    (@ (src
	"https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js")
       (integrity
	"sha384-vBWWzlZJ8ea9aCX4pEW3rVHjgjt7zpkNpZk+02D9phzyeVkE+jo0ieGizqPLForn")
       (crossorigin "anonymous"))))
