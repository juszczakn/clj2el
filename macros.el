(defun clj2el-convert-arr-to-list (arr)
  (mapcar (lambda (x) x) arr))

(defmacro fn (param-list body)
  (list 'lambda (clj2el-convert-arr-to-list param-list) body))

(defmacro def (name body)
  (list 'setf name body))

(defmacro defn (name param-list &optional body)
  (let* ((param-list (clj2el-convert-arr-to-list param-list)))
    (fset name `(lambda ,param-list ,body))))

(defmacro doc (sym)
  (list 'documentation sym))

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

(defmacro clj2el-let (param-array &optional body)
  (list 'let* (clj2el-partition-list (clj2el-convert-arr-to-list param-array))
        body))

(defmacro clj2el-map (f elts)
  (list 'mapcar f elts))

(defmacro str (&rest s)
  (list 'apply (quote 'concat) `(quote ,s)))
