;;; -*- lexical-binding: t -*-

;;; clj2el.el --- introducing some clojure-esque constructs to elisp

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

(require 'paredit)

;; Anything that clashes with current elisp namespace needs to go in here

;; -------------------------------------------------------------------------------------
;; macros 

(defun clj2el-convert-arr-to-list (arr)
  (mapcar (lambda (x) x) arr))

(defmacro fn (param-list body)
  (list 'lambda (clj2el-convert-arr-to-list param-list) body))

(defmacro def (name body)
  (list 'setf name body))

(defmacro defn (name param-list &rest body)
  (let ((param-list (clj2el-convert-arr-to-list param-list)))
    `(defun ,name ,param-list ,@body)))

(defmacro clj2el-macro (name param-list &rest body)
  (let ((param-list (clj2el-convert-arr-to-list param-list)))
    `(defmacro ,name ,param-list ,@body)))

(defun clj2el-partition-list (elts)
  (setf part-list nil)
  (while (not (equal nil elts))
    (let ((key (car elts))
          (val (cadr elts)))
      (setf part-list (if part-list (cons (list key val) part-list)
                        (list (list key val))))
      (setf elts (cddr elts))))
  (nreverse part-list))

(defmacro clj2el-make-hash-table (&rest contents)
  (let ((sym (gensym))
        (pd-contents (clj2el-partition-list contents)))
    (list 'progn
          `(setf ,sym (make-hash-table :test 'equal))
          `(progn ,@(mapcar (lambda (x) (list 'puthash (car x) (cadr x) sym)) pd-contents))
          sym)))

(defmacro clj2el-let (param-array &rest body)
  `(let* ,(clj2el-partition-list (clj2el-convert-arr-to-list param-array))
     ,@body))

(defmacro clj2el= (a b)
  `(equal ,a ,b))

(defmacro clj2el-map [f elts]
  (list 'mapcar f elts))

(defmacro clj2el-nth [coll index]
  (let [type (type-of coll)]
    `(if (= (type-of ,coll) (quote cons))
         ,(list 'nth index coll)
       (if (= (type-of ,coll) (quote vector))
           ,(list 'aref coll index)))))

;; -------------------------------------------------------------------------------------
;; reader

(defun clj2el-replace-= ()
  (beginning-of-buffer)
  (while (search-forward-regexp "([[:space:]]*nth" nil t)
    (replace-match "(clj2el-nth")))

(defun clj2el-replace-= ()
  (beginning-of-buffer)
  (while (search-forward-regexp "([[:space:]]*=" nil t)
    (replace-match "(clj2el=")))

(defun clj2el-replace-macro ()
  (beginning-of-buffer)
  (while (search-forward "defmacro" nil t)
    (replace-match "clj2el-macro")))

(defun clj2el-replace-map ()
  (beginning-of-buffer)
  (while (search-forward-regexp "\\bmap\\b" nil t)
    (replace-match "clj2el-map")))

(defun clj2el-replace-let ()
  (beginning-of-buffer)
  (while (search-forward-regexp "let[[:space:]]*\\[" nil t)
    (replace-match "clj2el-let [")))

(defun clj2el-replace-lambdas ()
  (beginning-of-buffer)
  (while (search-forward "#(" nil t)
    (replace-match "(lambda (&optional % %2 %3 %4) (")
    (backward-char)
    (paredit-forward)
    (insert ")")))

(defun clj2el-replace-hash-maps ()
  (beginning-of-buffer)
  (while (search-forward "{" nil t)
    (replace-match "(clj2el-make-hash-table "))
  (beginning-of-buffer)
  (while (search-forward "}" nil t)
    (replace-match ")")))

(defun clj2el-replace-essential-str ()
  (clj2el-replace-hash-maps)
  (clj2el-replace-lambdas)
  (clj2el-replace-let)
  (clj2el-replace-=)
  (clj2el-replace-map)
  (clj2el-replace-macro))

(defun clj2el-compile-buffer ()
  (let* ((cur-buf-substring (buffer-substring-no-properties 1 (point-max)))
         (cur-buf-name (buffer-file-name))
         (new-buf-name (concat cur-buf-name "j"))
         (new-buf (generate-new-buffer (generate-new-buffer-name "compilation-buffer"))))
    (set-buffer new-buf)
    (insert cur-buf-substring)
    (clj2el-replace-essential-str)
    (write-file new-buf-name)
    new-buf))

(defun clj2el-compile-buffer-and-eval ()
  (interactive)
  (let ((new-buf (clj2el-compile-buffer)))
    (eval-buffer)
    (kill-buffer new-buf)))

(defun clj2el-compile-buffer-dont-eval ()
  (interactive)
  (let ((new-buf (clj2el-compile-buffer)))
    (kill-buffer new-buf)))

;; -------------------------------------------------------------------------------------

(provide 'clj2el)

;; Local Variables:
;; Coding: utf-8
;; End:

;;; clj2el.el ends here
