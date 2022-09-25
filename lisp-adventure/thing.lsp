(defclass thing ()
  ((name :initarg :name
         :initform (error "thing must has name")
         :accessor thing-name)
   (desc :initarg :description
         :initform nil
         :accessor thing-description)
   (traits :initarg :traits
           :initform nil
           :accessor thing-traits)
   (owner :initarg :owner
          :initform nil
          :accessor thing-owner)
   (contents :initarg :contents
             :initform nil
             :accessor thing-contents)))


(macrolet ((thing-sym-reader (&rest accessors)
             (cons 'progn
                   (mapcar #'(lambda (accessor)
                               `(defmethod ,accessor ((sym symbol))
                                  (let ((thing (get-thing sym)))
                                    (,accessor thing))))
                           accessors))))
  (thing-sym-reader thing-name thing-description thing-traits thing-owner thing-contents))


(defun get-thing (thing-sym)
  (if (eq 'thing (type-of thing-sym))
      thing-sym
      (cdr (assoc thing-sym *things*))))


(defun get-owner-list (item-sym) ; gets all the things that own an item - as symbols - can be more
  (mapcar #'car
          (remove-if-not #'(lambda (thing-sym)
                             (member item-sym (thing-contents (car thing-sym))))
                         *things*)))

;; (defmethod thing-contents ((thing-sym symbol))
;;   (thing-contents (get-thing thing-sym)))

(defmethod (setf thing-contents) (value (thing-sym symbol))
  (setf (thing-contents (get-thing thing-sym)) value))

(defmethod has-trait ((thing thing) (trait symbol))
  (not (not (member trait (thing-traits thing)))))

(defmethod has-trait ((thing-sym symbol) (trait symbol))
  (has-trait (get-thing thing-sym) trait))


(defun split-! (trait-lst)
  (iflet it (position '! trait-lst)
    (list (subseq trait-lst 0 it)
          (subseq trait-lst (1+ it)))
    (list trait-lst nil)))

(defun has-traits (thing-sym trait-lst)
  (let* ((lst (split-! trait-lst))
         (yes (first lst))
         (no (second lst)))
    (and 
     (every (lambda (trait) (has-trait thing-sym trait)) yes)
     (every (lambda (trait) (not (has-trait thing-sym trait))) no))))

(defun add-trait (thing-sym trait)
  (let ((thing (get-thing thing-sym)))
    (with-accessors ((traits thing-traits)) thing
      (when (not (member trait traits))
        (setf traits (cons trait traits))))))

(defun del-trait (thing-sym trait)
  (let ((thing (get-thing thing-sym)))
    (with-accessors ((traits thing-traits)) thing
      (when (has-trait thing trait)
        (if (eq (car traits) trait)
            (setf traits (cdr traits))
            (del-from-down-lst traits trait))))))

(defun swap-trait (thing-sym old-trait new-trait)
  (del-trait thing-sym old-trait)
  (add-trait thing-sym new-trait))

(defun thing-exits (thing)
  (let ((thing (get-thing thing)))
    (mapcar #'car (remove-if-not (lambda (x) (eq (thing-name thing) (second x))) *go*))))

(defun thing-exits-desc (thing)
  (let ((exits (thing-exits thing)))
    (cond ((eq 1 (length exits))
           (format nil "A path leads towards the ~A." (tostr (car exits))))
          (exits
           (format nil "Paths lead towards")))))

(defmethod thing-desc ((x thing))
  (with-slots (name desc contents)
      x
    (let ((basic-desc (if desc desc
                          (format nil "This is a ~A." (tostr name)))))
      (when (has-trait x 'room)
        (if (has-trait x 'grass)
            (setf basic-desc (format nil "~A~%The place is overrun with overgrown grass." basic-desc))
            (let ((listables (find-thing-lst contents 'listable)))
              (dolist (i listables)
                (setf basic-desc (format nil "~A~%There is a ~A here." basic-desc
                                         (tostr i))))))
        (let ((exits (thing-exits name)))
          (when exits
            (setf basic-desc (format nil "~A~%Path~A lead to the ~A." basic-desc
                                     (if (> (length exits) 1) "s" "")
                                     (sym-lst-to-str-enum exits))))))
      (when (has-trait x 'furniture)
        (let ((listables (find-thing-lst contents 'listable)))
          (dolist (i listables)
            (setf basic-desc (format nil "~A~%There is a ~A inside." basic-desc
                                     (tostr i))))))
      (when (has-trait x 'growl)
        (setf basic-desc
              (format nil "~A~%You hear a deep, bone chilling growl close by." basic-desc)))
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
      (progn
        (setf *things* (cons (cons (thing-name thing) thing)
                             *things*))
        (setf *thing-syms* (cons sym *thing-syms*))))))

(defun thing-has (thing-sym item)
  (member item (thing-contents (get-thing thing-sym))))


(defun del-thing (obj-sym from)
  (let ((from (get-thing from)))
    (assert (member obj-sym (thing-contents from)) nil "del-thing failed to find contents ~A ~A ~%" obj-sym from)
    (setf (thing-contents from) (remove obj-sym (thing-contents from)))))


(defun move-thing (obj-sym from to)
  (del-thing obj-sym from)
  (add-to-thing obj-sym to))

(defun add-to-thing (item owner-sym)
  (setf (thing-owner (get-thing item)) owner-sym)
  (when (not (thing-has owner-sym item))
    (let ((thing (get-thing owner-sym)))
      (setf (thing-contents thing)
            (cons item (thing-contents thing))))))


(defun copy-thing (thing)
  (assert (eq (type-of thing) 'thing))
  (make-instance 'thing
                 :name (thing-name thing)
                 :description (thing-description thing)
                 :traits (copy-list (thing-traits thing))
                 :owner (thing-owner thing)
                 :contents (copy-list (thing-contents thing))))


(defun copy-thing-tree (tree)
  (cond ((consp tree)
         (cons (copy-thing-tree (car tree))
               (copy-thing-tree (cdr tree))))
        ((eq (type-of tree) 'thing)
         (copy-thing tree))
        (t tree)))

(defmacro with-saved-game-globals (&body body)
  `(let ,(mapcar #'(lambda (x) `(,x (copy-thing-tree ,x)))
                 '(*r* *go* *things* *death*))
     ,@body))

(defun tostr (sym)
  (let ((sym (iflet it (assoc sym *sym-to-str*)
               (cdr it) sym)))
   (if (symbolp sym)
       (string-downcase (string sym))
       sym)))

(defun make-thing (sym traits desc &key (contents nil) (owner nil))
  (returning ret (make-instance 'thing
                                :name sym
                                :traits traits
                                :description desc
                                :owner owner
                                :contents contents)
    (setf desc (format nil desc))
    (add-thing ret)
    (when owner
      (add-to-thing sym owner))
    (when contents
      (dolist (x contents)
        (add-to-thing x sym)))))


(defun find-thing-lst (thing-sym-lst &rest traits)
  (loop for sym in thing-sym-lst
        when (has-traits sym traits) collect sym))


