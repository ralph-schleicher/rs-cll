;;; rs-cll.lisp --- package definition.

;; Copyright (C) 2011--2013 Ralph Schleicher

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

(defpackage :rs-cll
  (:use :common-lisp
	:iterate)
  (:documentation
   "Ralph's Common Lisp library.")
  (:nicknames :rs)
  (:export ;; specials.lisp
	   #:optimize-for-speed
	   #:save-special-variables
	   #:restore-special-variables
	   ;; lisp/data.lisp
	   #:defconst
	   #:defsubst
	   #:false
	   #:true
	   #:nothing
	   ;; lisp/types.lisp
	   #:list-of-strings-p
	   #:list-of-strings
	   ;; lisp/conditions.lisp
	   #:ensure-type
	   ;; lisp/symbols.lisp
	   #:symbol-name*
	   ;; lisp/numbers.lisp
	   #:pi/6
	   #:pi/4
	   #:pi/2
	   #:2*pi
	   #:minf
	   #:maxf
	   #:addf
	   #:subf
	   #:mulf
	   #:divf
	   #:fma
	   #:fsd
	   #:fmaf
	   #:fsdf
	   #:float-epsilon
	   #:float-negative-epsilon
	   #:float-digits*
	   #:float-precision*
	   #:decimal-digits
	   #:absolute-ascending
	   #:absolute-descending
	   #:roman-numeral
	   ;; lisp/quantities.lisp
	   #:radian-from-degree
	   #:degree-from-radian
	   #:degree-from-sexagesimal
	   #:sexagesimal-from-degree
	   #:kelvin-from-degree-celsius
	   #:kelvin-from-degree-rankine
	   #:kelvin-from-degree-fahrenheit
	   #:degree-celsius-from-kelvin
	   #:degree-celsius-from-degree-rankine
	   #:degree-celsius-from-degree-fahrenheit
	   #:degree-rankine-from-kelvin
	   #:degree-rankine-from-degree-celsius
	   #:degree-rankine-from-degree-fahrenheit
	   #:degree-fahrenheit-from-kelvin
	   #:degree-fahrenheit-from-degree-celsius
	   #:degree-fahrenheit-from-degree-rankine
	   ;; lisp/characters.lisp
	   #:whitespace-char-p
	   #:blank-char-p
	   #:standard-alpha-char-p
	   #:standard-digit-char-p
	   ;; lisp/arrays.lisp
	   #:linear-index-from-subscripts
	   #:subscripts-from-linear-index
	   ;; lisp/strings.lisp
	   #:*random-string-alphabet*
	   #:random-string
	   ;; lisp/sequences.lisp
	   #:start-index-if
	   #:start-index-if-not
	   #:end-index-if
	   #:end-index-if-not
	   #:bounding-indices-if
	   #:bounding-indices-if-not
	   ;; lisp/streams.lisp
	   #:read-file
	   ;; lisp/reader
	   #:q-reader
	   ;; math/math.lisp
	   #:hypot
	   #:hypot3
	   #:real-sin
	   #:real-cos
	   #:cbrt
	   #:square
	   #:square-root
	   #:cube
	   #:cube-root
	   ;; math/brent.lisp
	   #:brent
	   ;; math/polynomials.lisp
	   #:evaluate-polynomial
	   #:quadratic-formula-1
	   #:quadratic-formula
	   #:cubic-formula-1
	   #:cubic-formula
	   ;; math/primes.lisp
	   #:modular-exponentiation
	   #:jacobi-symbol
	   #:is-prime
	   #:next-prime
	   #:next-prime*
	   #:previous-prime
	   #:previous-prime*
	   #:primes-between
	   #:nth-prime
	   #:prime-factors
	   ;; misc/regex.lisp
	   #:string-match
	   #:match-start
	   #:match-end
	   #:match-data
	   #:save-match-data
	   #:match-string
	   #:match-strings
	   #:replace-match
	   ;; misc/unicode.lisp
	   #:unicode-string-reader
	   ;; app/specials.lisp
	   #:standalone-program-p
	   ;; app/environ.lisp
	   #:environment-variables
	   #:environment-variable
	   ;; app/argv.lisp
	   #:program-invocation-name
	   #:program-invocation-short-name
	   #:program-arguments
	   ;; app/exit.lisp
	   #:exit-success
	   #:exit-failure
	   ;; app/diag.lisp
	   #:diagnostic-message
	   #:say
	   #:die
	   ;; app/work-dir.lisp
	   #:get-working-directory
	   #:set-working-directory
	   #:with-working-directory
	   ;; app/temp-dir.lisp
	   #:temporary-file-name
	   #:temporary-file
	   #:with-temporary-file
	   #:temporary-directory
	   #:with-temporary-directory
	   ;; app/getopt.lisp
	   #:getopt-error
	   #:unknown-option-name-error
	   #:ambiguous-option-name-error
	   #:missing-option-argument-error
	   #:superfluous-option-argument-error
	   #:optarg
	   #:optind
	   #:opterr
	   #:optopt
	   #:unprocessed-arguments
	   #:remaining-arguments
	   #:make-getopt
	   #:show-help-hint-and-die
	   #:getopt
	   ;; app/main.lisp
	   #:standalone-program
	   #:define-entry-point
	   ;; exec/exec.lisp
	   #:execute-program
	   #:with-input-from-program
	   #:with-output-to-program
	   #:with-program-io
	   #:program-input-stream
	   #:program-output-stream
	   #:program-error-stream
	   #:program-exit-status
	   #:close-program-streams))

;;; rs-cll.lisp ends here
