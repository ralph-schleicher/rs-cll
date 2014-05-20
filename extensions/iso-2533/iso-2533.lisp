;;; iso-2533.lisp --- ISO 2533 standard atmosphere.

;; Copyright (C) 2014 Ralph Schleicher

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

;;; Commentary:

;; All air properties are static quantities.

;;; Code:

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :iso-2533-derived-quantities *features*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :iso-2533-addendum-2 *features*))

(defpackage :iso-2533
  (:use :common-lisp
	:iterate
	:rs-cll)
  (:documentation "ISO 2533:1975 standard atmosphere.

Includes ISO 2533:1975/Add 1:1985 which provides hypsometrical
tables, that is geopotential altitude as a function of barometric
pressure.

Includes ISO 2533:1975/Add 2:1997 which extends the standard
atmosphere from -2000 m down to -5000 m geopotential altitude.

Since ISO 2533 is intended for use in calculations and design of
flying vehicles, we use the term \"altitude\" instead of \"height\"
or \"elevation\"."))

(in-package :iso-2533)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (save-special-variables
   (*read-default-float-format* 'double-float)))

;; True means to derive constant quantities via equations.
(define-symbol-macro derived-quantities (progn #+iso-2533-derived-quantities t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export 'standard-acceleration-of-gravity)
  (defconst standard-acceleration-of-gravity 9.80665
    "Standard acceleration of gravity at sea level in kilogram per square second.")

  (export 'standard-pressure)
  (defconst standard-pressure 101325.0
    "Standard air pressure at sea level in pascal.")

  (export 'standard-temperature)
  (defconst standard-temperature 288.15
    "Standard air temperature at sea level in kelvin.")

  (export 'standard-density)
  (defconst standard-density 1.225
    "Standard air density at sea level in kilogram per cubic meter.")

  (export 'avogadro-constant)
  (defconst avogadro-constant 6.02257E+23
    "Avogadro constant in one per mole.

The 2010 CODATA recommended value of this constant is 6.02214129E+23.
However, to reproduce the numerical values defined in ISO 2533 it is
required to utilize the value 6.02257E+23.")

  (export 'molar-gas-constant)
  (defconst molar-gas-constant 8.31432
    "Molar gas constant in joule per mole kelvin.

The 2010 CODATA recommended value of this constant is 8.3144621.
However, to reproduce the numerical values defined in ISO 2533 it
is required to utilize the value 8.31432.")

  (export 'specific-gas-constant)
  (defconst specific-gas-constant (if derived-quantities
				      (/ standard-pressure
					 standard-temperature
					 standard-density)
				    287.05287)
    "Specific gas constant of dry air in joule per kilogram kelvin.")

  (export 'molar-mass)
  (defconst molar-mass (if derived-quantities
			   (/ molar-gas-constant
			      specific-gas-constant)
			 0.028964420)
    "Molar mass of dry air in kilogram per mole.")

  (export 'ratio-of-specific-heats)
  (defconst ratio-of-specific-heats 1.4
    "Ratio of specific heats of dry air.")

  (export 'earth-radius)
  (defconst earth-radius 6356766.0
    "Radius of the earth in meter.")
  (values))

(define-symbol-macro NA avogadro-constant)
(define-symbol-macro R* molar-gas-constant)
(define-symbol-macro R specific-gas-constant)
(define-symbol-macro M molar-mass)
(define-symbol-macro gamma ratio-of-specific-heats)
(define-symbol-macro gn standard-acceleration-of-gravity)
;; Effective collision diameter of an air molecule in meter.
(define-symbol-macro sigma 0.365E-9)
;; Zero-point temperature of the Celsius temperature scale.
(define-symbol-macro To 273.15)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun p/pb (H Hb Tb L)
    "Calculate pressure ratio in an atmosphere layer.

First argument H is the geopotential altitude in meter.
Second argument HB is the geopotential altitude at base level in meter.
Third argument TB is the air temperature at base level in kelvin.
Fourth argument L is the lapse rate in kelvin per meter.

Value is the ratio of the air pressure to the air pressure at base level."
    (let ((dH (- H Hb)))
      (cond ((zerop dH)
	     1)
	    ((zerop L)
	     (exp (- (* (/ gn (* R Tb)) dH))))
	    (t
	     (expt (+ 1 (* (/ L Tb) dH)) (- (/ gn (* L R))))))))

  (defclass layer ()
    ((altitude
      :initarg :altitude
      :type double-float
      :documentation "Geopotential altitude at base level in meter.")
     (pressure
      :initarg :pressure
      :type double-float
      :documentation "Air pressure at base level in pascal.")
     (temperature
      :initarg :temperature
      :type double-float
      :documentation "Air temperature at base level in kelvin.")
     (lapse-rate
      :initarg :lapse-rate
      :type double-float
      :documentation "Air temperature lapse rate in kelvin per meter.")))

  (defconst layers (labels ((make-layer (Hb Tb L &optional neighbor)
			      ;; Air pressure at base level.
			      (let ((pb (if (not neighbor)
					    standard-pressure
					  (with-slots ((Hn altitude)
						       (pn pressure)
						       (Tn temperature)
						       (Ln lapse-rate)) neighbor
					    (if (> Hb Hn)
						(* pn (p/pb Hb Hn Tn Ln))
					      (/ pn (p/pb Hn Hb Tb L)))))))
				(make-instance 'layer
				  :altitude Hb
				  :pressure pb
				  :temperature Tb
				  :lapse-rate L))))
		     (let (b (n (make-layer     0 288.15 -0.0065)))
		       (vector
			#+iso-2533-addendum-2
			(setf b (make-layer -5000 320.65 -0.0065 n))
			#-iso-2533-addendum-2
			(setf b (make-layer -2000 301.15 -0.0065 n))
			n
			(setf b (make-layer 11000 216.65  0      n))
			(setf b (make-layer 20000 216.65  0.001  b))
			(setf b (make-layer 32000 228.65  0.0028 b))
			(setf b (make-layer 47000 270.65  0      b))
			(setf b (make-layer 51000 270.65 -0.0028 b))
			(setf b (make-layer 71000 214.65 -0.002  b))
			;; The mesopause starts at approximately 85 km.
			;; Therefore, continue with the lapse rate of
			;; the mesophere.
			(setf b (make-layer 80000 196.65 -0.002  b)))))
    "Sequence of atmosphere layers in increasing order of altitude.")

  (defconst last-layer-index (1- (length layers))
    "Index of the last atmosphere layer.")

  (defconst minimum-geopotential-altitude (slot-value (svref layers 0) 'altitude)
    "Minimum geopotential altitude in meter.")

  (defconst maximum-geopotential-altitude (slot-value (svref layers last-layer-index) 'altitude)
    "Maximum geopotential altitude in meter.")

  (defconst minimum-pressure-altitude (slot-value (svref layers last-layer-index) 'pressure)
    "Minimum pressure altitude in pascal.")

  (defconst maximum-pressure-altitude (slot-value (svref layers 0) 'pressure)
    "Maximum pressure altitude in pascal.")
  (values))

#-(and)
(defmethod print-object ((object layer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (altitude pressure temperature lapse-rate) object
      (princ (list altitude pressure temperature lapse-rate) stream))))

(defun find-layer (H)
  "Return layer object for a given geopotential altitude.

Argument H is the geopotential altitude in meter."
  (ensure-type H `(real ,minimum-geopotential-altitude
			,maximum-geopotential-altitude))
  (svref layers (iter (for j :from 1 :to last-layer-index)
		      (for layer = (svref layers j))
		      (when (< H (slot-value layer 'altitude))
			(return (1- j)))
		      (finally
		       (return last-layer-index)))))

(defun find-layer* (p)
  "Return layer object for a given pressure altitude.

Argument P is the air pressure in pascal."
  (ensure-type p `(real ,minimum-pressure-altitude
			,maximum-pressure-altitude))
  (svref layers (iter (for j :from 1 :to last-layer-index)
		      (for layer = (svref layers j))
		      (when (> p (slot-value layer 'pressure))
			(return (1- j)))
		      (finally
		       (return last-layer-index)))))

(export 'atm)
(defun atm (geopotential-altitude)
  "Calculate air pressure and air temperature as a function of altitude.

Argument GEOPOTENTIAL-ALTITUDE is the geopotential altitude in meter.

Values are the air pressure in pascal and the air temperature in
kelvin."
  (let* ((H geopotential-altitude)
	 (layer (find-layer H)))
    (with-slots ((Hb altitude)
		 (pb pressure)
		 (Tb temperature)
		 (L  lapse-rate)) layer
      (values
       ;; Air pressure.
       (* pb (p/pb H Hb Tb L))
       ;; Air temperature.
       (+ Tb (* L (- H Hb)))
       ))))

(export 'geometric-altitude)
(defun geometric-altitude (geopotential-altitude)
  "Convert altitude from geopotential altitude to geometric altitude.

Argument GEOPOTENTIAL-ALTITUDE is the geopotential altitude in meter.

Value is the geometric altitude in meter."
  (/ (* earth-radius geopotential-altitude)
     (- earth-radius geopotential-altitude)))

(export 'geopotential-altitude)
(defun geopotential-altitude (geometric-altitude)
  "Convert altitude from geometric altitude to geopotential altitude.

Argument GEOMETRIC-ALTITUDE is the geometric altitude in meter.

Value is the geopotential altitude in meter."
  (/ (* earth-radius geometric-altitude)
     (+ earth-radius geometric-altitude)))

(export 'flight-level)
(defun flight-level (number)
  "Convert altitude from flight level to geopotential altitude.

Argument NUMBER is the flight level in hundreds of feet.

Value is the geopotential altitude in meter."
  (* number 30.48))

(export 'pressure-altitude)
(defun pressure-altitude (pressure)
  "Convert altitude from barometric pressure to geopotential altitude.

Argument PRESSURE is the air pressure in pascal.

Value is the geopotential altitude in meter."
  (let* ((p pressure)
	 (layer (find-layer* p)))
    (with-slots ((Hb altitude)
		 (pb pressure)
		 (Tb temperature)
		 (L  lapse-rate)) layer
      (cond ((= p pb)
	     Hb)
	    ((zerop L)
	     (- Hb (* (/ (* R Tb) gn) (log (/ p pb)))))
	    (t
	     (+ Hb (* (/ Tb L) (- (exp (- (* (/ (* L R) gn) (log (/ p pb))))) 1)))
	     )))))

(export 'acceleration-of-gravity)
(defun acceleration-of-gravity (geometric-altitude)
  "Calculate acceleration of gravity as a function of altitude.

Argument GEOMETRIC-ALTITUDE is the geometric altitude in meter.

Value is the acceleration of gravity in kilogram per square second."
  (* gn (square (/ earth-radius (+ earth-radius geometric-altitude)))))

(export 'density)
(defun density (pressure temperature)
  "Calculate the density of air.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the air density in kilogram per cubic meter."
  (/ pressure R temperature))

(export 'specific-weight)
(defun specific-weight (geometric-altitude &optional pressure temperature)
  "Calculate the specific weight of air.

First argument GEOMETRIC-ALTITUDE is the geometric altitude in meter.
Second argument PRESSURE is the air pressure in pascal.
Third argument TEMPERATURE is the air temperature in kelvin.

Value is the specific weight of air in newton per cubic meter."
  (* (acceleration-of-gravity geometric-altitude)
     (density pressure temperature)))

(export 'number-density)
(defun number-density (pressure temperature)
  "Calculate the number density of air.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the number of air particles per cubic meter."
  (/ (* NA pressure)
     (* R* temperature)))

(export 'mean-speed)
(defun mean-speed (temperature)
  "Calculate the mean speed of an air particle, that is
the expected value of the Maxwell speed distribution.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the mean speed in meter per second."
  (* #.(sqrt (/ 8 pi)) (sqrt (* R temperature))))

(export 'mean-free-path)
(defun mean-free-path (pressure temperature)
  "Calculate the mean free path of an air particle.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the mean free path of an air particle in meter."
  ;; R*/NA is the Boltzmann constant.
  (* #.(/ R* NA (sqrt 2.0) pi (square sigma))
     (/ temperature pressure)))

(export 'collision-frequency)
(defun collision-frequency (pressure temperature)
  "Calculate the collision frequency of an air particle.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the collision frequency of an air particle in hertz."
  (* #.(* 4 NA (square sigma) (sqrt (/ pi R* M)))
     (/ pressure (sqrt temperature))))

(export 'speed-of-sound)
(defun speed-of-sound (temperature)
  "Calculate the speed of sound of air.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the speed of sound of air in meter per second."
  (sqrt (* gamma R temperature)))

(export 'dynamic-viscosity)
(defun dynamic-viscosity (temperature)
  "Calculate the dynamic viscosity of air.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the dynamic viscosity of air in pascal second.

The dynamic viscosity µ is calculated from the following empirical
formula:
                  -6  3/2
          1.458×10   T
     µ = -----------------
             T + 110.4

where T is the temperature."
  (/ (* 1.458E-6 (expt temperature 3/2))
     (+ temperature 110.4)))

(export 'kinematic-viscosity)
(defun kinematic-viscosity (pressure temperature)
  "Calculate the kinematic viscosity of air.

First argument PRESSURE is the air pressure in pascal.
Second argument TEMPERATURE is the air temperature in kelvin.

Value is the kinematic viscosity of air in square meter per second."
  (/ (dynamic-viscosity temperature)
     (density pressure temperature)))

(export 'thermal-conductivity)
(defun thermal-conductivity (temperature)
  "Calculate the thermal conductivity of air.

Argument TEMPERATURE is the air temperature in kelvin.

Value is the thermal conductivity of air in watt per meter kelvin.

The thermal conductivity λ is calculated from the following empirical
formula:
                     -3  3/2
          2.648151×10   T
     λ = --------------------
                      -12/T
          T + 245.4×10

where T is the temperature."
  (/ (* 2.648151E-3 (expt temperature 3/2))
     (+ temperature (* 245.4 (expt 10 (/ -12 temperature))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (restore-special-variables))

;;; iso-2533.lisp ends here
