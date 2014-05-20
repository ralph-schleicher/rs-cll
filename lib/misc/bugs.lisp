;;; bugs.lisp --- bug reports.

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
;;    * The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior
;;      written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS
;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Code:

(in-package :rs-cll)

(defconst +bug-report-address+
  (let ((sys (asdf:find-system :rs-cll)))
    (or (ignore-errors
	  (asdf:system-maintainer sys))
	(ignore-errors
	  (asdf:system-author sys))
	(format nil "<~A@~A>"
		"rs" "ralph-schleicher.de")))
  "Mail address or URL for reporting bugs.")

(defun fix-me (&optional symbol type)
  "Issue a feature improvement request."
  (error "~A for ~A on ~A ~A.~%~
Please send patches to ~A."
	 (if (null symbol)
	     "This functionality is not implemented"
	   (format nil "The ~A `~A' is not defined"
		   (or type (if (fboundp symbol)
				"function"
			      "symbol"))
		   (symbol-name* symbol)))
	 (lisp-implementation-type)
	 (software-type)
	 (machine-type)
	 +bug-report-address+))

;;; bugs.lisp ends here
