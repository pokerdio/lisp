 
(defun del-from-down-lst (lst sym)
  "destructively deletes an item from a list except from the first element"
  (when (cdr lst)
    (if (eq sym (cadr lst))
        (setf (cdr lst) (cddr lst))
        (del-from-down-lst (cdr lst) sym))))

(defmacro returning (varname init-form &body body)
  `(let ((,varname ,init-form))
     (progn 
       ,@body
       ,varname)))

(defmacro iflet (varname condition-form &body body)
  `(let ((,varname ,condition-form))
     (if ,varname ,@body)))

(defmacro p (&rest args)
  `(format t (cat ,@args)))





(defun sym-to-low-str (sym)
  (if (symbolp sym)
      (string-downcase (string sym))
      sym))

(defun sym-lst-to-str-enum (sym-lst)
  (let ((sym-lst (mapcar #'sym-to-low-str sym-lst)))
    (cond ((eq 1 (length sym-lst))
           (sym-to-low-str (car sym-lst)))
          ((eq 2 (length sym-lst))
           (format nil "~A and ~A"
                   (sym-to-low-str (first sym-lst))
                   (sym-to-low-str (second sym-lst))))
          (sym-lst
           (let ((first (first sym-lst))
                 (last (car (last sym-lst)))
                 (mid (cdr (butlast sym-lst))))
             (format nil "~A~{, ~A~} and ~A" first mid last))))))


(defun cat (&rest args)
  (format nil "~{~a~^~}" (mapcar #' sym-to-low-str args)))


(defun keyword-wrap (lst)
  (setf lst (append lst (list :the-end)))
  (let ((ret nil)
        (constr-lst nil))
    (dolist (x lst)
      (cond ((and constr-lst (keywordp x))
             (setf ret (cons constr-lst ret))
             (setf constr-lst (list x)))
            ((keywordp x)
             (setf constr-lst (list x)))
            (constr-lst
             (setf constr-lst (append constr-lst (list x))))
            (t (setf ret (cons x ret)))))
    (reverse ret)))


(defmacro while-let (var test &body body)
  `(do ((,var ,test ,test)) ((not ,var) (values))
     ,@body))

(defmacro while (test &body decls/tags/forms)
  `(do () ((not ,test) (values))
     ,@decls/tags/forms))

(defun lst-starts-with (seq subseq &optional (test-fun #'eq))
  (or (null subseq)
      (and seq
           (or (null subseq)
               (and (funcall test-fun (car seq) (car subseq))
                    (lst-starts-with (cdr seq) (cdr subseq) test-fun))))))

(defun lst-replace (seq src dest)
  (assert (and src dest))
  (cond ((< (length seq) (length src))
         seq)
        ((lst-starts-with seq src)
         (let ((seq2
                 (append dest
                         (nthcdr (length src) seq))))
           (assert (not (equal seq seq2)))
           (lst-replace seq2 src dest)))
        (t (cons (car seq)
                 (lst-replace (cdr seq) src dest)))))



