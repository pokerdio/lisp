(defparameter *t* '())
(defparameter *r* 'house-s)
(defparameter *f* '())
(defparameter *ignore-tokens*
  '("" "the" "an" "a"))

(defparameter *pc* '())

(defparameter *translate*
  '((n . north) (s . south) (e . east) (w . west)
    (u . up) (d . down)
    (q . quit)
    (l . look)
    (x . look)
    (examine . look)
    (see . look)
    (watch . look)
    (q . quit)
    (o . open)
    (i . inventory)))

(defparameter *multi-translate*
  '(((climb down) . (go down))
    ((climb up) . (go up))
    ((foo bar baz) . (foo bar))
    ((work bench) . (bench))))

(defparameter *go* '())

(defparameter *command-handled* nil)
(defparameter *bound-var* nil)
(defparameter *things* nil)

(defun reverse-dir (dir)
  (cond ((eq dir 'east) 'west)
        ((eq dir 'south) 'north)
        ((eq dir 'north) 'south)
        ((eq dir 'west) 'east)
        ((eq dir 'up) 'down)
        ((eq dir 'down) 'up)))


(defun trans (sym)
  (if (assoc sym *translate*)
      (cdr (assoc sym *translate*))
      sym))
