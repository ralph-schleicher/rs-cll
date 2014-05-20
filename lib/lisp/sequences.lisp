;;; sequences.lisp --- sequences.

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

(export 'start-index-if)
(defun start-index-if (predicate seq &key (start 0) end key)
  "Return start index of first element in SEQ matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

If no element matches PREDICATE, return the end index position.
Likewise if SEQ is empty."
  (or (position-if predicate seq :start start :end end :key key) end (length seq)))

(export 'start-index-if-not)
(defun start-index-if-not (predicate seq &key (start 0) end key)
  "Return start index of first element in SEQ not matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

If all elements match PREDICATE, return the end index position.
Likewise if SEQ is empty."
  (or (position-if-not predicate seq :start start :end end :key key) end (length seq)))

(export 'end-index-if)
(defun end-index-if (predicate seq &key (start 0) end key)
  "Return end index of last element in SEQ matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

If no element matches PREDICATE, return the start index position.
Likewise if SEQ is empty."
  (let ((pos (position-if predicate seq :from-end t :start start :end end :key key)))
    (if pos (1+ pos) start)))

(export 'end-index-if-not)
(defun end-index-if-not (predicate seq &key (start 0) end key)
  "Return end index of last element in SEQ not matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

If all elements match PREDICATE, return the start index position.
Likewise if SEQ is empty."
  (let ((pos (position-if-not predicate seq :from-end t :start start :end end :key key)))
    (if pos (1+ pos) start)))

(export 'bounding-indices-if)
(defun bounding-indices-if (predicate seq &key (start 0) end key)
  "Return start index of first element in SEQ and end index of last element
in SEQ matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

If no element matches PREDICATE, return the end index positions.
Likewise if SEQ is empty."
  (when (null end)
    (setf end (length seq)))
  (let* ((left (or (position-if predicate seq :start start :end end :key key) end))
	 (right (position-if predicate seq :from-end t :start left :end end :key key)))
    (setf right (if right (1+ right) start))
    (if (< left right)
	(values left right)
      (values end end))))

(export 'bounding-indices-if-not)
(defun bounding-indices-if-not (predicate seq &key (start 0) end key)
  "Return start index of first element in SEQ and end index of last element
in SEQ not matching PREDICATE.

Keywords START and END are bounding index designators.
Keyword KEY is a function designator of one argument.

If all elements match PREDICATE, return the end index positions.
Likewise if SEQ is empty."
  (when (null end)
    (setf end (length seq)))
  (let* ((left (or (position-if-not predicate seq :start start :end end :key key) end))
	 (right (position-if-not predicate seq :from-end t :start left :end end :key key)))
    (setf right (if right (1+ right) start))
    (if (< left right)
	(values left right)
      (values end end))))

;;; sequences.lisp ends here
