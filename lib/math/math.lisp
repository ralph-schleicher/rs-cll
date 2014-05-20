;;; math.lisp --- mathematics.

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

(export 'cbrt)
(if (realp (expt -8 1/3))
    (defsubst cbrt (number)
      (declare (type number number))
      (expt number 1/3))
  (defun cbrt (number)
    (declare (type number number))
    (if (realp number)
	(* (signum number) (expt (abs number) 1/3))
      (expt number 1/3))))
(setf (documentation 'cbrt 'function)
      "Return the cube root of NUMBER.

If NUMBER is a real number, value is the real cube root of NUMBER.")

(export 'hypot)
(defsubst hypot (x y)
  "Return the distance between a point and the origin
in a two-dimensional Cartesian coordinate system.

Arguments X and Y have to be real numbers."
  (declare (type real x y))
  (abs (complex x y)))

(export 'hypot3)
(defun hypot3 (x y z)
  "Return the distance between a point and the origin
in a three-dimensional Cartesian coordinate system.

Arguments X, Y, and Z have to be real numbers."
  (declare (type real x y z))
  (setf x (abs x)
	y (abs y)
	z (abs z))
  ;; Scale by largest element.
  (let ((s (max x y z)))
    (when (/= s 0)
      (setf x (/ x s)
	    y (/ y s)
	    z (/ z s)))
    (* s (sqrt (+ (* x x) (* y y) (* z z))))))

(export 'square)
(defsubst square (z)
  "Return Z squared, that is Z raised to the power two.

Argument Z has to be a number."
  (declare (type number z))
  (* z z))

(export 'square-root)
(if (rationalp (sqrt 4/9))
    (progn
      (defsubst square-root (z)
	(declare (type number z))
	(sqrt z)))
  (defun square-root (z)
    (declare (type number z))
    (cond ((zerop z)
	   (if (rationalp z)
	       0
	     (float 0 (abs z))))
	  ((and (realp z) (plusp z))
	   (let ((f (sqrt z)))
	     (if (rationalp z)
		 (let ((r (rationalize f)))
		   (if (or (= r f) (= (square r) z)) r f))
	       f)))
	  (t
	   (sqrt z)))))
(setf (documentation 'square-root 'function)
      "Return the square root of Z.

Argument Z has to be a number.

The `square-root' function attempts to propagate the type
of the argument Z to its value.")

(export 'cube)
(defsubst cube (z)
  "Return Z cubed, that is Z raised to the power three.

Argument Z has to be a number."
  (declare (type number z))
  (* z z z))

(export 'cube-root)
(defun cube-root (z)
  "Return the cube root of Z.

Argument Z has to be a number.

If argument Z is zero, value is zero.  If argument Z is
a real number, value is the real cube root of Z.

The `cube-root' function attempts to propagate the type
of the argument Z to its value."
  (declare (type number z))
  (cond ((zerop z)
	 (if (rationalp z)
	     0
	   (float 0 (abs z))))
	((realp z)
	 (let ((f (* (signum z) (expt (abs z) 1/3))))
	   (if (rationalp z)
	       (let ((r (rationalize f)))
		 (if (or (= r f) (= (cube r) z)) r f))
	     f)))
	(t
	 (expt z 1/3))))

;;; math.lisp ends here
