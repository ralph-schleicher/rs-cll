;;; rs-cll.asd --- ASDF system definition.

;; Copyright (C) 2012 Ralph Schleicher

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in
;;      the documentation and/or other materials provided with the
;;      distribution.
;;
;;    * Neither the name of the copyright holder nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :common-lisp-user)

(asdf:defsystem :rs-cll
  :description "Ralph's Common Lisp library."
  :author "Ralph Schleicher <rs@ralph-schleicher.de>"
  :license "Modified BSD License"
  :version "20160430.1353"
  :depends-on (:trivial-features :alexandria :iterate :uiop :cl-ppcre :babel)
  :serial t
  :components ((:file "rs-cll")
	       (:file "specials")
	       ;; Portable Common Lisp extensions.
	       (:module "lisp"
		:serial t
		:components ((:file "data")
			     (:file "types")
			     (:file "conditions")
			     (:file "symbols")
			     (:file "numbers")
			     (:file "quantities")
			     (:file "characters")
			     (:file "arrays")
			     (:file "strings")
			     (:file "sequences")
			     (:file "streams")
			     (:file "reader")))
	       (:module "math"
		:serial t
		:components ((:file "math")
			     (:file "brent")
			     (:file "polynomials")
			     (:file "primes")))
	       (:module "misc"
		:serial t
		:components ((:file "regex")
			     (:file "unicode")
			     (:file "bugs")))
	       ;; Application support.
	       (:module "app"
		:serial t
	   	:components ((:file "specials")
			     (:file "environ")
			     (:file "argv")
			     (:file "exit")
			     (:file "diag")
			     (:file "work-dir")
			     (:file "temp-dir")
			     (:file "getopt")
			     (:file "main")))
	       (:module "exec"
		:serial t
	   	:components ((:file "exec")
			     #+ccl
			     (:file "exec-ccl")
			     #+clisp
			     (:file "exec-clisp")
			     #+ecl
			     (:file "exec-ecl")
			     #+sbcl
			     (:file "exec-sbcl")))))

;; local variables:
;; time-stamp-time-zone: "UTC"
;; time-stamp-format: "%:y%02m%02d.%02H%02M"
;; time-stamp-start: ":version\\s-+\""
;; time-stamp-end: "\""
;; end:

;;; rs-cll.asd ends here
