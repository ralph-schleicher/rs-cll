;;; quantities.lisp --- quantities and units.

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

(in-package :rs-cll)

(defsubst radian-from-degree (deg)
  "Convert a plane angle from degree to radian.

Argument DEG is the angle given in degree.

Value is the corresponding angle given in radian."
  (declare (type real deg))
  (* (/ deg 180) pi))

(defsubst degree-from-radian (rad)
  "Convert a plane angle from radian to degree.

Argument RAD is the angle given in radian.

Value is the corresponding angle given in degree."
  (declare (type real rad))
  (* (/ rad pi) 180))

(defun degree-from-sexagesimal (deg &optional (min 0) (s 0) (sign 1))
  "Join sexagesimal subdivisions of an arc degree into a plane angle.

First argument DEG is the number of arc degrees.
Optional second argument MIN is the number of arc minutes.
Optional third argument S is the number of arc seconds.
Optional fourth argument SIGN specifies the sign.

Arguments DEG, MIN, and S have to be non-negative numbers.

Value is the corresponding angle given in degree.  If none of the
arguments is a floating-point number, value is a rational number."
  (declare (type (real 0) deg min s) (type real sign))
  (let ((angle (+ (/ (+ (/ s 60) min) 60) deg)))
    (if (or (floatp angle) (floatp sign))
	(float-sign (float sign) (float angle (float sign)))
      (if (minusp sign) (- angle) angle))))

(defun sexagesimal-from-degree (angle)
  "Split a plane angle into sexagesimal subdivisions of an arc degree.

Argument ANGLE is the angle given in degree.

Return values are the arc degrees, arc minutes, and arc seconds.  The
number of arc seconds may be a floating point number with fractions of
a second.  Fourth value is either plus one or minus one specifying the
sign of the angle."
  (declare (type real angle))
  (let (sign deg min s f)
    (setf sign (if (floatp angle)
		   (float-sign angle)
		 (if (minusp angle) -1 +1)))
    (multiple-value-setq (deg f)
      (floor (abs angle)))
    (multiple-value-setq (min f)
      (floor (* f 60)))
    (multiple-value-setq (s f)
      (floor (* f 60)))
    (values deg min (if (zerop f) s (+ s f)) sign)))

;;;; Temperature

(defun kelvin-from-degree-celsius (value)
  "Convert temperature from degree Celsius to kelvin."
  (+ value 27315/100))

(defun kelvin-from-degree-rankine (value)
  "Convert temperature from degree Rankine to kelvin."
  (/ value 18/10))

(defun kelvin-from-degree-fahrenheit (value)
  "Convert temperature from degree Fahrenheit to kelvin."
  (/ (+ value 45967/100) 18/10))

(defun degree-celsius-from-kelvin (value)
  "Convert temperature from kelvin to degree Celsius."
  (- value 27315/100))

(defun degree-celsius-from-degree-rankine (value)
  "Convert temperature from degree Rankine to degree Celsius."
  (- (/ value 18/10) 27315/100))

(defun degree-celsius-from-degree-fahrenheit (value)
  "Convert temperature from degree Fahrenheit to degree Celsius."
  (/ (- value 32) 18/10))

(defun degree-rankine-from-kelvin (value)
  "Convert temperature from kelvin to degree Rankine."
  (* value 18/10))

(defun degree-rankine-from-degree-celsius (value)
  "Convert temperature from degree Celsius to degree Rankine."
  (* (+ value 27315/100) 18/10))

(defun degree-rankine-from-degree-fahrenheit (value)
  "Convert temperature from degree Fahrenheit to degree Rankine."
  (+ value 45967/100))

(defun degree-fahrenheit-from-kelvin (value)
  "Convert temperature from kelvin to degree Fahrenheit."
  (- (* value 18/10) 45967/100))

(defun degree-fahrenheit-from-degree-celsius (value)
  "Convert temperature from degree Celsius to degree Fahrenheit."
  (+ (* value 18/10) 32))

(defun degree-fahrenheit-from-degree-rankine (value)
  "Convert temperature from degree Rankine to degree Fahrenheit."
  (- value 45967/100))

;;; quantities.lisp ends here
