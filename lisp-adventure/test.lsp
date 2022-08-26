(defun profit (low-price high-price)
  (* 100 
	 (/ (- (* (- 1.0 0.04604) high-price)
		   (* 1.0104 low-price))
		high-price)))



