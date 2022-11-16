(defun die (message)
  (p message)
  (p "~%You die.~%")
  (setf *death* t))

(defun go-room (room msg)
  (p msg)
  (setf *r* room)
  (p (thing-desc *r*)))

(defun continue-command ()
  (setf *command-handled* nil))

(defun find-connection (dir room)
  (loop for (d r1 r2) in *go*
        when (and (eq d dir) (eq r1 room))
          return r2))

(defun connect-1-way (place1 dir place2)
  (setf dir (trans dir))
  (setf *go* (cons (list dir place1 place2) *go*)))

(defun connect (place1 dir place2)
  (setf dir (trans dir))
  (connect-1-way place1 dir place2)
  (connect-1-way place2 (reverse-dir dir) place1))

(defun expand-triplets-by-two (lst)
  (when (cdr lst)
    (cons (list (car lst) (cadr lst) (caddr lst))
          (expand-triplets-by-two (cddr lst)))))

(defun quote-every (lst)
  (when lst
    (cons (list 'quote (car lst))
          (quote-every (cdr lst)))))

(defmacro trail (&rest syms)
  (cons 'progn
        (mapcar #'(lambda (x) (cons 'connect (quote-every x)))
                (expand-triplets-by-two syms))))

(defmacro trail-1-way (&rest syms)
  (cons 'progn
        (mapcar #'(lambda (x) (cons 'connect-1-way (quote-every x)))
                (expand-triplets-by-two syms))))

(defun var? (sym)
  (member sym '(x y z a b c d e f g h i j k l m n o p q r s u v w)))

(defun free-var? (var)
  (and (var? var)
       (not (bound-var? var))))

(defun bound-var? (var)
  ;; (p (cat "bound-var? " var " " *bound-var* "~%"))
  (and (var? var)
       (member var *bound-var*)))

(defun tokenize-string (s)
  (setf s (uiop:split-string s :separator '(#\  #\Tab #\, #\! #\? #\. #\" #\+ #\; #\: #\` #\~ #\')))
  (setf s (remove-if #'(lambda (x) (member (string-downcase x)
                                           *ignore-tokens* :test #'string=))
                     s))
  (setf s (mapcar #'(lambda (x)
                      (intern (string-upcase x))) s))
  (dolist (v *multi-translate*)
    (setf s (lst-replace s (car v) (cdr v))))
  (setf s (mapcar #'(lambda (x)
                      (let ((a (assoc x *translate*)))
                        (if a (cdr a) x)))
                  s))
  s)



(defun vars-in-pat (pat)
  "returns the list of variables in a pattern, sorted alphabetically"
  (sort (loop for x in pat
              when (var? x)
                collect x)
        #'string-lessp))

(defun neighbors-different (pat)
  "checks if all the elements in pat are different from adjacent elements - \
intended use with sorted lists"
  (cond ((not pat) t)
        ((not (cdr pat)) t)
        ((not (equal (car pat) (cadr pat)))
         (neighbors-different (cdr pat)))
        (t nil)))


(defun all-same (pats)
  "checks if all elements are equal"
  (if (or (not pats) (not (cdr pats))) t
      (every #'(lambda (x) (equal x (car pats)))
             (cdr pats))))

(defun valid-match-list (pats)
  "makes sure a list of match patterns has the same variable sets, \
 with no repeating variables"
  (assert pats) ; no empties plox
  (let ((var (mapcar #'vars-in-pat pats)))
    (and (neighbors-different (car var))
         (all-same var))))


(defun game-read-line ()
  (format t "> ")
  (read-line))

(defun build-match-var-wrap (input-sym var body)
  (if (bound-var? var)
      `(when (and ,input-sym (eq (car ,input-sym) ,var))
         (let ((,input-sym (cdr ,input-sym)))
           ,@(funcall body)))
      (let ((*bound-var* (cons var *bound-var*)))
        `(when ,input-sym
           (let ((,var (car ,input-sym))
                 (,input-sym (cdr ,input-sym)))
             ,@(funcall body))))))

(defun build-match-const-wrap (input-sym c body)
  `(when (and ,input-sym (eq (car ,input-sym) ',c))
     (let ((,input-sym (cdr ,input-sym)))
       ,@(funcall body))))

(defun build-match-var-opt-wrap (input-sym v-form body)
  (let ((var (car v-form))
        (opt (cdr v-form)))
    `(when (and ,input-sym (member (car ,input-sym) ',opt))
       (let ((,var (car ,input-sym))
             (,input-sym (cdr ,input-sym)))
         ,@(funcall body)))))

(defun build-match-var-const-wrap (input-sym const-list body)
  `(when (and ,input-sym (member (car ,input-sym) ',const-list))
     (let ((,input-sym (cdr ,input-sym)))
       ,@(funcall body))))

(defun build-match-in-room-wrap (room-lst body)
  (if (var? (car room-lst))
      (progn
        (assert (member (car room-lst) *bound-var*) nil
                "cannot only match room objects in :in-room clause, once per")
        (let ((*bound-var* (cons (car room-lst) *bound-var*)))
          `(when (has-traits *r* ',(cdr room-lst))
             (let ((,(car room-lst) *r*)))
             ,@(funcall body))))
      `(when (member *r* ',room-lst)
         ,@(funcall body))))

(defun dasein ()
  (thing-contents (get-thing *r*)))

(defun having ()
  (thing-contents (get-thing 'pc)))

(defun having-or-dasein ()
  (append (thing-contents (get-thing *r*))
          (thing-contents (get-thing 'pc))))


(defun build-match-dasein-const-wrap (dasein-lst body)
  `(when (intersection ',dasein-lst (dasein))
     ,@(funcall body)))

(defun atom-or-car (lst)
  (mapcar #'(lambda (x) (let ((x (if (consp x) (car x) x)))
                          (if (var? x)
                              (prog1 x
                                (assert (member x *bound-var*) nil "free vars not allowed here"))
                              (list 'quote x))))
          lst))

;;; TODO

(defun wrap-room-data-trait-match (trait pat body)
  (let* ((freevars (remove-if-not #'free-var? pat))
         (n (length pat))
         (*bound-var* (append *bound-var* freevars))
         (body (funcall body)))
    (assert (equal freevars (remove-duplicates freevars)) ()
            "duplicate variable in the data pattern ~A of trait ~A~%" pat trait)
    `((when (and (eq (length (trait-value *r* ',trait)) ,(length pat))
                 ,@(loop for i from 0 to (1- (length pat))
                         as item = (elt pat i)
                         when (not (member item freevars))
                           collect `(equal ,(if (var? item) item `(quote ,item)) ; quote syms not (bound) vars
                                           (elt (trait-value *r* ',trait) ,i))))
        ,@(if freevars
              `((symbol-macrolet
                    ,(mapcar (lambda (var)
                               `(,var (elt (trait-value *r* ',trait)
                                           ,(position var pat))))
                      freevars)
                  ,@body))
              body)))))

(defun wrap-room-data-trait-lst-match (trait-lst body)
  (let* ((trait-lst (remove-if-not #'consp trait-lst))
         (current-trait (car trait-lst)))
    (if trait-lst
        (wrap-room-data-trait-match (car current-trait) (cdr current-trait)
                                    (lambda ()
                                      (wrap-room-data-trait-lst-match
                                       (cdr trait-lst) body)))
        (funcall body))))

(defun build-room-trait (trait-lst body)
  (let ((traits (atom-or-car trait-lst)))
    `(when (has-traits *r* (list ,@traits))
       ,@(wrap-room-data-trait-lst-match trait-lst body))))

(defun quotify-traits-lst (traits)
  `(list ,@(mapcar #'(lambda (x)
                       (cond ((bound-var? x) x)
                             ((var? x) (assert nil))
                             ((symbolp x) (list 'quote x))
                             (t (assert nil))))
                   traits)))

(defun build-match-thing-not (thing-lst body place-exp)
  (let ((item (car thing-lst))
        (traits (cdr thing-lst)))
    (if (var? item)
        (assert (bound-var? item) nil "negative match requires bound variable ~A~%" thing-lst)
        (setf item (list 'quote item)))
    
    (if traits
        `(when (or (not (member ,item ,place-exp))
                   (not (has-traits (car (member ,item ,place-exp))
                                    ,(quotify-traits-lst traits))))
           ,@(funcall body))
        `(when (not (member ,item ,place-exp))
           ,@(funcall body)))))

(defun build-match-thing-const-wrap (thing-lst body place-exp)
  (let ((item (car thing-lst))
        (traits (cdr thing-lst)))
    (if traits
        `(when (and (member ',item ,place-exp)
                    (has-traits ',item ,(quotify-traits-lst traits)))
           ,@(funcall body))
        `(when (member ',item ,place-exp)
           ,@(funcall body)))))

(defun build-match-thing-var-wrap (thing-lst body place-exp)
  (let ((var (car thing-lst))
        (traits (cdr thing-lst))
        (traits-list (quotify-traits-lst (cdr thing-lst))))
    (assert (var? var))
    (cond ((and (bound-var? var) (null traits))
           `(when (member ,var ,place-exp)
              ,@(funcall body)))
          ((and (bound-var? var) traits)
           `(when (and (member ,var ,place-exp)
                       (has-traits ,var ,traits-list))
              ,@(funcall body)))
          (traits
           (let ((*bound-var* (cons var *bound-var*)))
             `(dolist (,var ,place-exp)
                (when (and (not *command-handled*)
                           (has-traits ,var ,traits-list))
                  ,@(funcall body)))))
          (t (let ((*bound-var* (cons var *bound-var*)))
               `(dolist (,var ,place-exp)
                  (when (not *command-handled*)
                    ,@(funcall body))))))))


(defun build-match-thing (thing-lst body place-exp)
  (assert thing-lst)
  (if (var? (car thing-lst))
      (build-match-thing-var-wrap thing-lst body place-exp)
      (build-match-thing-const-wrap thing-lst body place-exp)))



(defun build-inside-check (item container body)
  (let ((item (if (var? item)
                  item (list 'quote item)))
        (container (if (var? container)
                       container (list 'quote container))))
    `(when (and (get-thing ,item)
                (get-thing ,container)
                (member ,item (thing-contents ,container)))
       ,@(funcall body))))





(defun build-match-lambda-body (input-sym pat body)
  (p 'build-match-lambda-body " " input-sym " "  pat) 
  (when (listp body)
    (let ((body-double body))
      (setf body (lambda () body-double))))

  (if pat
      (let ((var1 (car pat))
            (rest-body (lambda ()
                         (list
                          (build-match-lambda-body input-sym (cdr pat) body)))))
        (let ((ret
                (cond ((eq '* var1)
                       (assert (every #'consp (cdr pat)) () "* must be last in a pattern")
                       `(let ((* ,input-sym)
                              (,input-sym nil))
                          ,@(funcall rest-body)))
                      ((var? var1)
                       (build-match-var-wrap input-sym var1 rest-body))
                      ((and var1 (listp var1) (var? (car var1)))
                       (build-match-var-opt-wrap input-sym var1 rest-body))
                      ((and var1 (listp var1) (eq :in-room (car var1)))
                       (build-match-in-room-wrap (cdr var1) rest-body))

                      ((and var1 (listp var1) (eq :dasein (car var1)))
                       (build-match-thing (cdr var1) rest-body '(dasein)))
                      
                      ((and var1 (listp var1) (eq :having (car var1)))
                       (build-match-thing (cdr var1) rest-body '(having)))
                      
                      ((and var1 (listp var1) (eq :thing (car var1)))
                       (build-match-thing (cdr var1) rest-body '*thing-syms*))
                      
                      ((and var1 (listp var1) (eq :having-or-dasein (car var1)))
                       (build-match-thing (cdr var1) rest-body '(having-or-dasein)))

                      ((and var1 (listp var1) (eq :not-having (car var1)))
                       (build-match-thing-not (cdr var1) rest-body '(having)))

                      ((and var1 (listp var1) (eq :not-there (car var1)))
                       (build-match-thing-not (cdr var1) rest-body '(dasein)))                      

                      ((and var1 (listp var1) (eq :inside (car var1)))
                       (assert (eq (length var1) 3) nil ":inside needs exactly two arguments, not ~A" var1)
                       (let ((a (second var1))
                             (b (third var1)))
                         (when (var? a)
						   (assert (member a *bound-var*) nil
                                   ":inside deals only with bound variables, not ~A in ~A" a var1))
                         (when (var? b)
						   (assert (member b *bound-var*) nil
                                   ":inside deals only with bound variables, not ~A in ~A" b var1))
                         (build-inside-check a b rest-body)))

                      ((and var1 (listp var1) (eq :room-trait (car var1)))
                       (build-room-trait (cdr var1) rest-body))
                      
                      ((and var1 (listp var1))
                       (build-match-var-const-wrap input-sym var1 rest-body))
                      (t (build-match-const-wrap input-sym var1 rest-body)))))
          ret))
      `(when (not ,input-sym) ,@(funcall body))))



;; (defmacro match-com (pat &body body)
;;   (let* ((after-com (member :after pat))
;;          (flist (if after-com '*f-after* '*f*))
;;          (body (if after-com body
;;                    (cons '(setf *command-handled* t) body)))
;;          (pat (keyword-wrap (remove :after pat)))
;;          (com-sym (gensym))
;;          (*bound-var* nil))
;;     `(progn
;;        (setf ,flist
;;              (append ,flist
;;                      (list (lambda (,com-sym)
;;                              ;; (p "attempting match command " ',pat " <" ,com-sym "> ~%")
;;                              ,(build-match-lambda-body com-sym pat body))))))))

(defun add-rule (alist rule-lambda)
  (setf (cdr (last alist)) (list rule-lambda)))

(defmacro match-com (pat &body body)
  (let* ((after-com (member :after pat))
         (flist (if after-com '*f-after* '*f*))
         (body (if after-com body
                   (cons '(setf *command-handled* t) body)))
         (pat (keyword-wrap (remove :after pat)))
         (com-sym (gensym))
         (*bound-var* nil))
    `(progn
       (add-rule ,flist
                 (lambda (,com-sym)
                   ;; (p "attempting match command " ',pat " <" ,com-sym "> ~%")
                   ,(build-match-lambda-body com-sym pat body))))))

(defun split-before-first (lst &optional (test (lambda (x) (not (consp x)))))
  (cond ((not lst) (values nil nil))
        ((funcall test (car lst)) (values nil (copy-list lst)))
        (t (multiple-value-bind (a b) (split-before-first (cdr lst) test)
             (values (cons (car lst) a) b)))))

(defun match-comms-unfurl (pats)
  (multiple-value-bind (a b)
      (split-before-first pats)
    (mapcar (lambda (lst) (append lst b)) a)))

(defmacro match-coms (pats &body body)
  (let ((pats (match-comms-unfurl pats)))
    `(progn
       ,@(loop for pat in pats
               collect `(match-com ,pat ,@body)))))

(defun process-commands (com)
  (let ((*command-handled* nil))
    (do ((f *f* (cdr f)))
        ((or (not f) *command-handled*) nil)
      (funcall (car f) com)))
  (do ((f *f-after* (cdr f)))
      ((not f) nil)
    (funcall (car f) com)))

(defun game-loop ()
  (with-saved-game-globals
    (p (thing-desc *r*))
    (terpri)
    (do ((com (tokenize-string (game-read-line))
              (if *death* '(quit) (tokenize-string (game-read-line)))))
        ((equal com '(quit)) t)
      (process-commands com)
      (terpri))))

(flet ((replace-second (new-value lst)
         (cons (car lst)
               (cons new-value (cddr lst)))))
  (defmacro block-match (block-name match-clauses &body body)
    `(block ,block-name
       ,@(mapcar #'(lambda (elm)
                     (if (member (car elm) '(match-com match-coms))
                         (replace-second (append (second elm) match-clauses) elm)
                         elm))
                 body))))
