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

(use-modules((guix licenses) #:prefix license:)
	    (gnu packages)
	    (gnu packages bash)
	    (gnu packages autotools)
	    (gnu packages pkg-config)
	    (gnu packages databases)
	    (gnu packages guile)
	    (guix build-system gnu)
	    (guix utils)
	    (guix packages)
	    (guix git-download)
	    (guix gexp))

(package
 (name "pastebin")
 (version "0.0")
 (source (origin
	  (method git-fetch)
	  (uri (git-reference
		(url "https://github.com/kristoferbuffington/pastebin.git")
		(commit "05040eb8064e32a45ec94b70dd3c78d22b6761eb")))
	  (sha256
	   (base32
	    "1irwz6plhbkaly3x6hbjyz3yxjbk58qw9nqnbg045sf04d0j9an5"))))
 (build-system gnu-build-system)
 (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _
             (zero? (system* "sh" "bootstrap")))))))
 (inputs `(("guile" ,guile-2.2)
	   ("guile-lib" ,guile-lib)
	   ;("guile-json" ,guile-json)
	   ("guile-wiredtiger" ,guile-wiredtiger)
	   ("guile-fibers" ,guile-fibers)))
 (native-inputs `(("pkgconfig"  ,pkg-config)
		  ("autoconf" ,autoconf)
		  ("automake" ,automake)))
 (synopsis "Pastebin web app written in Guile")
 (description "Pastebin web app written in Guile")
 (home-page "https://github.com/kristoferbuffington/pastebin")
 (license (list license:gpl3+ license:lgpl3+)))
