;;; -*- lexical-binding: t -*-

;;; clj2el-core.el --- introducing some clojure-esque core constructs to elisp

;; Copyright (c) 2015 juszczakn
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;;

;;; Code:

(require 'clj2el)

;; -------------------------------------------------------------------------------------
;; macros 

(defmacro fn (param-list body)
  `(lambda ,(clj2el-convert-arr-to-list param-list) ,body))

(defmacro def (name body)
  `(setf ,name ,body))

(defmacro defn (name param-list &rest body)
  (let ((param-list (clj2el-convert-arr-to-list param-list)))
    `(defun ,name ,param-list ,@body)))

(defmacro doc (sym)
  `(documentation ,sym))

(defmacro str (&rest s)
  `(apply 'concat (quote ,s)))

(defmacro not= (a b)
  `(not (equal ,a ,b)))

;; -------------------------------------------------------------------------------------

(provide 'clj2el-core)

;; Local Variables:
;; Coding: utf-8
;; End:

;;; clj2el-core.el ends here
