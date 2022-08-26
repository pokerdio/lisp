(defclass thing ()
  ((name :initarg :name
         :initform (error "thing must has name")
         :accessor thing-name)
   (desc :initarg :description
         :initform nil)
   (traits :initarg :traits
           :initform nil
           :accessor thing-traits)
   (owner :initarg :owner
          :initform nil
          :accessor thing-owner)
   (alias :initarg :alias
          :initform nil
          :accessor thing-alias)
   (contents :initarg :contents
             :initform nil
             :accessor thing-contents)))


(defun get-thing (thing-sym)
  (if (eq 'thing (type-of thing-sym))
      thing-sym
      (cdr (assoc thing-sym *things*))))

(defmethod has-trait ((thing thing) (trait symbol))
  (not (not (member trait (thing-traits thing)))))

(defmethod has-trait ((thing-sym symbol) (trait symbol))
  (has-trait (get-thing thing-sym) trait))

(defun has-traits (thing-sym trait-lst)
  (every (lambda (trait) (has-trait thing-sym trait)) trait-lst))

(defun add-trait (thing-sym trait)
  (let ((thing (get-thing thing-sym)))
    (with-accessors ((traits thing-traits)) thing
      (when (not (member trait traits))
        (setf traits (cons trait traits))))))

(defun del-trait (thing-sym trait)
  (let ((thing (get-thing thing-sym)))
    (with-accessors ((traits thing-traits)) thing
      (when (has-trait trait thing)
        (if (eq (car traits) trait)
            (setf traits (cdr traits))
            (del-from-down-lst traits trait))))))

(defmethod thing-desc ((x thing))
  (with-slots (name desc contents)
      x
    (let ((basic-desc (if desc desc
                          (format nil "This is a ~A." (sym-to-low-str name)))))
      (when (has-trait x 'room)
        (let ((listables (find-thing-lst contents 'listable)))
          (dolist (i listables)
            (setf basic-desc (format nil "~A~%There is a ~A here." basic-desc (sym-to-low-str i))))))
      basic-desc)))

(defmethod thing-desc ((sym symbol))
  (let ((thing (get-thing sym)))
    (thing-desc thing)))

(defmethod print-object ((x thing) out)
  (format out "thing:~A" (thing-name x)))

(defmethod detail-thing ((x thing))
  (format t "thing(~A|~A|~A)"
          (thing-name x)
          (thing-traits x)
          (thing-desc x)))


;; (remove-method #'detail-object
;;                (find-method #'detail-object '() (list (find-class 'thing) t)))

(defun add-thing (thing)
  (let ((sym (thing-name thing)))
    (iflet it (assoc sym *things*)
      (setf (cdr it) thing)
      (setf *things* (cons (cons (thing-name thing) thing)
                           *things*)))))

(defun thing-has (thing-sym item)
  (member item (thing-contents (get-thing thing-sym))))


(defun del-thing (obj-sym from)
  (let ((from (get-thing from)))
    (assert (member obj-sym (thing-contents from)))
    (setf (thing-contents from) (remove obj-sym (thing-contents from)))))

(defun add-to-thing (item thing-sym)
  (setf (thing-owner (get-thing item)) thing-sym)
  (when (not (thing-has thing-sym item))
    (let ((thing (get-thing thing-sym)))
      (setf (thing-contents thing)
            (cons item (thing-contents thing))))))

(defun make-thing (sym traits desc &key (contents nil) (owner nil) (alias nil))
  (returning ret (make-instance 'thing
                                :name sym
                                :traits traits
                                :description desc
                                :owner owner
                                :alias alias
                                :contents contents)
    (add-thing ret)
    (when owner
      (add-to-thing sym owner))
    (when contents
      (dolist (x contents)
        (add-to-thing x sym)))))

(defun find-thing-lst (thing-sym-lst &rest traits)
  (loop for sym in thing-sym-lst
        when (has-traits sym traits) collect sym))
