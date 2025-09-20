(defparameter +ranks+ '(2 3 4 5 6 7 8 9 10 J Q K A))
(defparameter +suits+ '(:clubs :diamonds :hearts :spades))
(defparameter +suit_unicode+ '((:spades . "♠") 
			       (:hearts . "♥")
			       (:diamonds . "♦")
			       (:clubs . "♣")))

(defun suit-symbol (suit)
  (cdr (assoc suit +suit_unicode+)))

(defparameter +cards+)
(defparameter +hands+)

(defclass card ()
  ((rank :initarg :rank :accessor card-rank)
   (suit :initarg :suit :accessor card-suit)
   (text :accessor card-text :initform nil)))

(defmethod initialize-instance :after ((c card) &key)
  (with-slots (rank suit text) c
    (setf text (format nil "~A~A" rank (suit-symbol suit)))))

(defun printem (&rest args)
    (dolist (el args) 
        (princ el) 
        (princ #\SPACE))
  (princ #\NEWLINE))

(progn
  (setf +cards+ (make-hash-table))
  (setf +hands+ (make-hash-table))
  (loop for r in +ranks+ do 
    (loop for s_char in '("C" "D" "H" "S") 
	  for s in +suits+ do 
	    ;; (let ((card (intern (format nil "~A~A" ))))
	    ;;   (setf (gethash card +cards+)
	    ;; 	    (make-instance 'card :suit s :rank r))
	    ;;   ;;     (print `(defparameter ,card (gethash ,card +cards+)) )
	    ;;   )
	    (printem r s s_char)
	  )))
