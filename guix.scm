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
	    (guix gexp))

(package
 (name "gnupaste")
 (version "0.0")
 (source (local-file "." #:recursive? #t))
 (build-system gnu-build-system)
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
 (home-page "https://paste.freshbakedyams.com")
 (license (list license:gpl3+ license:lgpl3+)))
