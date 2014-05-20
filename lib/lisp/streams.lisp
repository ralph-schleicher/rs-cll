;;; streams.lisp --- streams.

;; Copyright (C) 2013 Ralph Schleicher

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

(export 'read-file)
(defun read-file (&optional (input-stream *standard-input*) element-length)
  "Read all elements from a stream.

Optional first argument INPUT-STREAM has to be an open input stream.
 The default is standard input.
Optional second argument ELEMENT-LENGTH is the size of an element in
 the external format given in byte.  This argument has no effect for
 binary streams.

If the element type of the stream is a character, value is a string.
Otherwise, value is a vector.

The ELEMENT-LENGTH argument may be used to speed up reading from the
stream by allocating the resulting sequence all at once.  This only
works if the file size of the stream can be determined, too.  If the
external format has a variable length element size like, for example,
the UTF-8 character encoding, then ELEMENT-LENGTH should be the
minimum length of an element.  In such a case, more memory than
actually needed may be allocated for the return value.

Reading a text file:

     (with-open-file (s ... :direction :input)
       (read-file s))

     (with-open-file (s ... :direction :input :external-format :UTF-8)
       (read-file s 1))

     (with-open-file (s ... :direction :input :external-format :UCS-2BE)
       (read-file s 2))

Reading a binary file:

     (with-open-file (s ... :direction :input :element-type '(unsigned-byte 8))
       (read-file s))"
  (ensure-type element-length '(or null (rational (0))))
  (let* ((file-length (ignore-errors (file-length input-stream)))
	 (element-type (stream-element-type input-stream))
	 (text-mode (subtypep element-type 'character)))
    (if (not text-mode)
	(setf element-length 1)
      ;; Always use 'with-output-to-string' on SBCL.
      #+(or sbcl)
      (setf element-length nil))
    (cond ((and file-length element-length)
	   (let* ((elements (ceiling (/ file-length element-length)))
		  (seq (make-array elements :element-type element-type :fill-pointer t)))
	     (setf (fill-pointer seq) (read-sequence seq input-stream))
	     (values seq)))
	  (text-mode
	   (let* ((buffer-size (max 512 (min (or file-length 4096) 65536)))
		  (buffer (make-array buffer-size :element-type element-type)))
	     (with-output-to-string (string nil :element-type element-type)
	       (iter (for count = (read-sequence buffer input-stream))
		     (declare (type fixnum count))
		     (while (> count 0))
		     (write-sequence buffer string :end count)))))
	  (t
	   (let* ((buffer-size (max 512 (min (or file-length 4096) 65536)))
		  (buffer (make-array buffer-size :element-type element-type))
		  (seq (make-array 0 :element-type element-type :adjustable t :fill-pointer t)))
	     (iter (for count = (read-sequence buffer input-stream))
		   (declare (type fixnum count))
		   (while (> count 0))
		   (iter (for index :from 0 :below count)
			 (declare (type fixnum index))
			 (vector-push-extend (aref buffer index) seq)))
	     (adjust-array seq (length seq))))
	  ))))

;;; streams.lisp ends here
