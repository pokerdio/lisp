(defun profit (low-price high-price)
  (* 100 
	 (/ (- (* (- 1.0 0.04604) high-price)
		   (* 1.0104 low-price))
		high-price)))



(defun reverse-list (lst &optional visited)
  (let ((visited (or visited 
                     (make-hash-table))))
   (if (null lst)
       nil
       (if (gethash lst visited)
           nil
           (progn
             (setf (gethash lst visited) t)
             (append (reverse-list (cdr lst) visited) (list (car lst))))))))


(defun circular-list (lst)
  (rotatef  (last lst) (car lst))
  lst)
