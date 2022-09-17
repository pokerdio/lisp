(defun die (message)
  (p message)
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

(defun bound-var? (var)
  (p (cat "bound-var? " var " " *bound-var* "~%"))
  (and (var? var)
       (member var *bound-var*)))

(defun tokenize-string (s)
  (setf s (uiop:split-string s :separator '(#\  #\Tab #\, #\! #\? #\.)))
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
        (assert (not (member (car room-lst) *bound-var*)) nil
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


;; todo: turn the command pattern from, to:
;; look :dasein frog 
;; look (:dasein frog)


(defun build-match-dasein-const-wrap (dasein-lst body)
  `(when (intersection ',dasein-lst (dasein))
     ,@(funcall body)))

(defun build-room-trait (trait-lst body)
  `(when (has-traits *r* ',trait-lst)
     ,@(funcall body)))

(defun quotify-traits-lst (traits)
  `(list ,@(mapcar #'(lambda (x)
                       (cond ((bound-var? x) x)
                             ((var? x) (assert nil))
                             ((symbolp x) (list 'quote x))
                             (t (assert nil))))
                   traits)))

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
                (cond ((var? var1)
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



(defmacro match-com (pat &body body)
  (let ((pat (keyword-wrap pat))
        (com-sym (gensym))
        (body (cons '(setf *command-handled* t) body ))
        (*bound-var* nil))
    `(progn
       (setf *f*
             (append *f*
                     (list (lambda (,com-sym)
                             ,(build-match-lambda-body com-sym pat body))))))))

(defun match-comms-unfurl (pats)
  (setf pats (append pats '(())))
  (let ((ret nil)
        (temp nil)
        (sym-buf nil))
    (dolist (x pats)
      (cond ((and (listp x) sym-buf)
             (setf ret (append ret
                               (mapcar #'(lambda (pat)
                                           (append pat sym-buf))
                                       temp)))
             (setf temp (list x))
             (setf sym-buf nil))
            ((listp x)
             (setf temp (append temp (list x))))
            ((symbolp x)
             (setf sym-buf
                   (append sym-buf (list x))))))
    (append ret (butlast temp))))

(defmacro match-coms (pats &body body)
  (let ((pats (match-comms-unfurl pats)))
    `(progn
       ,@(loop for pat in (remove-if #'symbolp pats)
               collect `(match-com ,pat ,@body)))))

(defun process-commands (com)
  (let ((*command-handled* nil))
    (do ((f *f* (cdr f)))
        ((or (not f) *command-handled*) nil)
      (funcall (car f) com))))

(defun game-loop ()
  (with-saved-game-globals
    (p (thing-desc *r*))
    (terpri)
    (do ((com (tokenize-string (game-read-line))
              (if *death* '(quit) (tokenize-string (game-read-line)))))
        ((equal com '(quit)) t)
      (process-commands com)
      (terpri))))
