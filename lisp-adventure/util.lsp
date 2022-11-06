 
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

(defun sym-lst-to-str-enum (sym-lst)
  (let ((sym-lst (mapcar #'tostr sym-lst)))
    (cond ((eq 1 (length sym-lst))
           (tostr (car sym-lst)))
          ((eq 2 (length sym-lst))
           (format nil "~A and ~A"
                   (tostr (first sym-lst))
                   (tostr (second sym-lst))))
          (sym-lst
           (let ((first (first sym-lst))
                 (last (car (last sym-lst)))
                 (mid (cdr (butlast sym-lst))))
             (format nil "~A~{, ~A~} and ~A" first mid last))))))


(defun cat (&rest args)
  (format nil "~{~a~^~}" (mapcar #' tostr args)))


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

(defun remove-nth (n lst)
  (if (> n 0)
      (when lst (cons (car lst)
                      (remove-nth (1- n) (cdr lst))))
      (cdr lst)))



(flet ((pop-random (lst)
         (let* ((n (random (length lst)))
                (elt (nth n lst))
                (rest (remove-nth n lst)))
           (values elt rest)))
       (neigh-lst (row col max-row max-col)
         (let (ret)
           (when (> row 0)
             (setf ret (cons (list 'north (1- row) col) ret)))
           (when (> col 0)
             (setf ret (cons (list 'west row (1- col)) ret)))
           (when (< row (1- max-row))
             (setf ret (cons (list 'south (1+ row) col) ret)))
           (when (< col (1- max-col))
             (setf ret (cons (list 'east row (1+ col)) ret)))
           ret)))

  (defun make-labyrinth ()
    (let* ((m 5) (n 5) 
           (v (make-array (list m n) :initial-element nil))
           (ret (make-array (list m n) :initial-element nil))         
           (open '((0 1) (1 0))))
      (setf (aref v 0 0) 'entry)
      (dotimes (i (1- (* m n)))
        (multiple-value-bind (expand new-open) (pop-random open)
          (let* ((row (first expand))
                 (col (second expand))
                 (src (pop-random (remove-if-not
                                   (lambda (x) (aref v (second x) (third x)))
                                   (neigh-lst row col m n)))))
            (setf (aref v row col) (first src))
            (setf open (remove-duplicates
                        (append new-open
                                (remove-if
                                 (lambda (x) (aref v (car x) (cadr x)))
                                 (mapcar #'cdr (neigh-lst row col m n))))
                        :test #'equal)))))
      (dotimes (row m)
        (dotimes (col n)
          (let ((dir (aref v row col)))
            (when (member dir '(east west north south))
              (push dir (aref ret row col)))
            (case dir
              (east (push 'west (aref ret row (1+ col))))
              (west (push 'east (aref ret row (1- col))))
              (south (push 'north (aref ret (1+ row) col)))
              (north (push 'south (aref ret (1- row) col)))))))
      (push 'east (aref ret (1- m) (1- n)))
      (push 'west (aref ret 0 0))
      ret)))



(defun dir-abs-to-rel (dirx diry dir-sym)
  (cdr
   (case (+ dirx (* 2 diry))
     (1 (assoc dir-sym '((east . forward) (north . left) (west . backward) (south . right)))) ;east
     (-2 (assoc dir-sym '((east . right) (north . forward) (west . left) (south . backward)))) ; north
     (-1 (assoc dir-sym '((east . backward) (north . right) (west . forward) (south . left)))) ; west
     (2 (assoc dir-sym '((east . left) (north . backward) (west . right) (south . forward))))))) ; south
