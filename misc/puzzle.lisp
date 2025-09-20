(defun printem (&rest args)
    (dolist (el args) 
        (princ el) 
        (princ #\SPACE))
  (format t "~%"))

(defun printhash (hash) (maphash #'printem hash))
(defparameter +forward_count+ (make-hash-table))
(defparameter +back_count+ (make-hash-table))
(defparameter +forward_list+ (make-hash-table))
(defparameter +back_list+ (make-hash-table))

(defun inc-hash (key table)
  (setf (gethash key table) 
	(1+ (gethash key table 0))))

(defun push-hash (key table value)
  (setf (gethash key table)
	(cons value (gethash key table)))) 

(loop for i from 2 to 17 do
      (loop for j from i to 17 do
	(let ((sum (+ i j))
	      (prod (* i j)))
	  (inc-hash sum +forward_count+)
	  (inc-hash prod +back_count+)
	  (push-hash sum +forward_list+ prod)
	  (push-hash prod +back_list+ sum))))

(defun prod-knows (prod) (eq (gethash prod +back_count+) 1))

(maphash (lambda (prod count) (printem prod count (prod-knows prod))) +back_count+)


(defun know-count (prod-lst)
  (let ((len (length prod-lst))
	(knows (count-if #'prod-knows prod-lst)))
    (- len knows)))

(maphash (lambda (sum lst) (when (eq 1 (know-count lst)) (printem sum lst))) +forward_list+)
