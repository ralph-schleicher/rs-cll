;;; iso-639-data.lisp --- language data.

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

(in-package :iso-639)

(define-language "ara"
  ((:part-2/b "ara")
   (:part-1 "ar")
   (:short-name
    :ara #U(#|العربية|#
	    216 167 217 132 216 185 216 177 216 168 217 138 216 169)
    :deu "Arabisch"
    :eng "Arabic")))

(define-language "bul"
  ((:part-2/b "bul")
   (:part-1 "bg")
   (:short-name
    :bul #U(#|Български|#
	    208 145 209 138 208 187 208 179 208 176 209 128 209 129 208 186 208 184)
    :deu "Bulgarisch"
    :eng "Bulgarian")))

(define-language "ces"
  ((:part-2/b "cze")
   (:part-2/t "ces")
   (:part-1 "cs")
   (:short-name
    :ces #U(#|Čeština|#
	    196 140 101 197 161 116 105 110 97)
    :deu "Tschechisch"
    :eng "Czech")))

(define-language "dan"
  ((:part-2/b "dan")
   (:part-1 "da")
   (:short-name
    :dan "Dansk"
    :deu #U(#|Dänisch|#
	    68 195 164 110 105 115 99 104)
    :eng "Danish")))

(define-language "nld"
  ((:part-2/b "dut")
   (:part-2/t "nld")
   (:part-1 "nl")
   (:short-name
    :deu #U(#|Niederländisch|#
	    78 105 101 100 101 114 108 195 164 110 100 105 115 99 104)
    :eng "Dutch"
    :nld "Nederlands")))

(define-language "eng"
  ((:part-2/b "eng")
   (:part-1 "en")
   (:short-name
    :deu "Englisch"
    :eng "English")))

(define-language "est"
  ((:part-2/b "est")
   (:part-1 "et")
   (:short-name
    :deu "Estnisch"
    :eng "Estonian"
    :est "Eesti")))

(define-language "fin"
  ((:part-2/b "fin")
   (:part-1 "fi")
   (:short-name
    :deu "Finnisch"
    :eng "Finnish"
    :fin "Suomi")))

(define-language "fra"
  ((:part-2/b "fre")
   (:part-2/t "fra")
   (:part-1 "fr")
   (:short-name
    :deu #U(#|Französisch|#
	    70 114 97 110 122 195 182 115 105 115 99 104)
    :eng "French"
    :fra #U(#|Français|#
	    70 114 97 110 195 167 97 105 115))))

(define-language "deu"
  ((:part-2/b "ger")
   (:part-2/t "deu")
   (:part-1 "de")
   (:short-name
    :deu "Deutsch"
    :eng "German")))

(define-language "gle"
  ((:part-2/b "gle")
   (:part-1 "ga")
   (:short-name
    :deu "Irisch"
    :eng "Irish"
    :gle "Gaeilge")))

(define-language "ell"
  ((:part-2/b "gre")
   (:part-2/t "ell")
   (:part-1 "el")
   (:short-name
    :deu "Griechisch"
    :ell #U(#|Ελληνικά|#
	    206 149 206 187 206 187 206 183 206 189 206 185 206 186 206 172)
    :eng "Greek")))

(define-language "hrv"
  ((:part-2/b "hrv")
   (:part-1 "hr")
   (:short-name
    :deu "Kroatisch"
    :eng "Croatian"
    :hrv "Hrvatski")))

(define-language "hun"
  ((:part-2/b "hun")
   (:part-1 "hu")
   (:short-name
    :deu "Ungarisch"
    :eng "Hungarian"
    :hun "Magyar")))

(define-language "ita"
  ((:part-2/b "ita")
   (:part-1 "it")
   (:short-name
    :deu "Italienisch"
    :eng "Italian"
    :ita "Italiano")))

(define-language "lav"
  ((:part-2/b "lav")
   (:part-1 "lv")
   (:short-name
    :deu "Lettisch"
    :eng "Latvian"
    :lav #U(#|Latviešu|#
	    76 97 116 118 105 101 197 161 117))))

(define-language "lit"
  ((:part-2/b "lit")
   (:part-1 "lt")
   (:short-name
    :deu "Litauisch"
    :eng "Lithuanian"
    :lit #U(#|Lietuvių|#
	    76 105 101 116 117 118 105 197 179))))

(define-language "mlt"
  ((:part-2/b "mlt")
   (:part-1 "mt")
   (:short-name
    :deu "Maltesisch"
    :eng "Maltese"
    :mlt "Malti")))

(define-language "mon"
  ((:part-2/b "mon")
   (:part-1 "mn")
   (:short-name
    :deu "Mongolisch"
    :eng "Mongolian"
    :mon #U(#|Монгол|#
	    208 156 208 190 208 189 208 179 208 190 208 187))))

(define-language "pol"
  ((:part-2/b "pol")
   (:part-1 "pl")
   (:short-name
    :deu "Polnisch"
    :eng "Polish"
    :pol "Polski")))

(define-language "por"
  ((:part-2/b "por")
   (:part-1 "pt")
   (:short-name
    :deu "Portugiesisch"
    :eng "Portuguese"
    :por #U(#|Português|#
	    80 111 114 116 117 103 117 195 170 115))))

(define-language "ron"
  ((:part-2/b "rum")
   (:part-2/t "ron")
   (:part-1 "ro")
   (:short-name
    :deu #U(#|Rumänisch|#
	    82 117 109 195 164 110 105 115 99 104)
    :eng "Romanian"
    :ron #U(#|Română|#
	    82 111 109 195 162 110 196 131))))

(define-language "rus"
  ((:part-2/b "rus")
   (:part-1 "ru")
   (:short-name
    :deu "Russisch"
    :eng "Russian"
    :rus #U(#|Русский|#
	    208 160 209 131 209 129 209 129 208 186 208 184 208 185))))

(define-language "slk"
  ((:part-2/b "slo")
   (:part-2/t "slk")
   (:part-1 "sk")
   (:short-name
    :deu "Slowakisch"
    :eng "Slovak"
    :slk #U(#|Slovenčina|#
	    83 108 111 118 101 110 196 141 105 110 97))))

(define-language "slv"
  ((:part-2/b "slv")
   (:part-1 "sl")
   (:short-name
    :deu "Slowenisch"
    :eng "Slovenian"
    :slv #U(#|Slovenščina|#
	    83 108 111 118 101 110 197 161 196 141 105 110 97))))

(define-language "spa"
  ((:part-2/b "spa")
   (:part-1 "es")
   (:short-name
    :deu "Spanisch"
    :eng "Spanish"
    :spa #U(#|Español|#
	    69 115 112 97 195 177 111 108))))

(define-language "swe"
  ((:part-2/b "swe")
   (:part-1 "sv")
   (:short-name
    :deu "Schwedisch"
    :eng "Swedish"
    :swe "Svenska")))

;;;; Special codes.

(define-language "mis"
  ((:part-2/b "mis")
   (:short-name
    :deu "Einzelne andere Sprachen"
    :eng "Uncoded languages")))

(define-language "mul"
  ((:part-2/b "mul")
   (:short-name
    :deu "Mehrere Sprachen"
    :eng "Multiple languages")))

(define-language "und"
  ((:part-2/b "und")
   (:short-name
    :deu "Nicht zu entscheiden"
    :eng "Undetermined")))

(define-language "zxx"
  ((:part-2/b "zxx")
   (:short-name
    :deu "Kein linguistischer Inhalt"
    :eng "No linguistic content")))

;;; iso-639-data.lisp ends here
