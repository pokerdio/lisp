
(defpackage :range-tex
  (:use :cl)
  (:export :render-range-to-tex))
(in-package :range-tex)

;(defparameter *ranks* (reverse '("A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2")))

(defparameter *ranks* (reverse '("A" "K" "Q" "J" "T")))

(defun hand-name (col row)
  "col = 0..12 left→right (A..2), row = 0..12 top→bottom (A..2).
   Above diagonal => suited, below => offsuit, diag => pair."
  (let* ((r1 (nth col *ranks*))
         (r2 (nth row *ranks*)))
    (cond
      ((= col row) (format nil "~A~A" r1 r2))            ; e.g., AA, TT, 22
      ((> col row) (format nil "~A~As" r1 r2))           ; e.g., AKs
      (t            (format nil "~A~Ao" r2 r1)))))       ; e.g., AKo

(defun tex-escape (s)
  "Minimal TeX escaping for common specials."
  (let ((table '(("\\" . "\\textbackslash{}")
                 ("{" . "\\{") ("}" . "\\}")
                 ("$" . "\\$")
                 ("#" . "\\#")
                 ("%" . "\\%")
                 ("&" . "\\&")
                 ("_" . "\\_")
                 ("~" . "\\textasciitilde{}")
                 ("^" . "\\textasciicircum{}"))))
    (labels ((esc1 (str pair) (with-output-to-string (o)
                                (loop with from = (car pair)
                                      with to = (cdr pair)
                                      for i = 0 then (1+ i)
                                      for ch across str
                                      do (if (and (stringp from) (string= (string ch) from))
                                             (princ to o)
                                             (princ ch o))))))
      (reduce #'esc1 table :initial-value (princ-to-string s)))))

(defun cell-block (title lines)
  ;; Generates: \shortstack[l]{\textbf{AKs}\\CALL\\5\\SHOVE\\10\\FOLD}
  (with-output-to-string (s)
    (format s "\\shortstack[l]{\\textbf{~a}" (tex-escape title))
    (dolist (ln lines)
      (format s "\\\\ \\tiny ~a" (tex-escape ln)))
    (format s "}")))

(defun build-table-rows (hand->lines)
  "hand->lines: alist/hash where key is hand-name (\"AKs\", \"TT\", \"57o\")
   and value is a list of strings like (\"CALL\" \"5\" \"SHOVE\")."
  (labels ((lookup (k)
             (etypecase hand->lines
               (hash-table (gethash k hand->lines))
               (list (cdr (assoc k hand->lines :test #'string=))))))
    (loop for row from 0 to (1- (length *ranks*)) collect
      (let ((cells
              (loop for col from 0 to (1- (length *ranks*)) collect
                (let* ((hand (hand-name col row))
                       (lines (or (lookup hand) '())))
		  (print (list 'POKEY hand (lookup hand) lines))
                  (cell-block hand lines)))))
        ;; Join 13 cells with & and end the row with \\ \hline
        (format nil "~{~a~^ & ~} \\\\ \\hline~%" cells)))))

(defun make-tex-document (rows)
  "rows is a list of strings, each is a TeX row line."
  (with-output-to-string (s)
    (format s "\\documentclass[a4paper]{article}~%")
    (format s "\\usepackage[margin=10mm]{geometry}~%")
    (format s "\\usepackage{array}~%")
    (format s "\\usepackage{graphicx}~%")
    (format s "\\setlength{\\tabcolsep}{2pt}~%")
    (format s "\\renewcommand{\\arraystretch}{1.15}~%")
    (format s "\\begin{document}~%\\thispagestyle{empty}~%")
    ;; Make each column exactly 1/13 of textwidth
    (format s "\\newlength{\\cellw}\\setlength{\\cellw}{\\dimexpr\\textwidth/13\\relax}~%")
    (format s "{\\footnotesize~%")
    (format s "\\noindent\\begin{tabular}{|*{13}{p{\\cellw}|}}\\hline~%")
    (dolist (row rows) (princ row s))
    (format s "\\end{tabular}~%}~%")
    (format s "\\end{document}~%")))

(defun write-string-to-file (path string)
  (with-open-file (f path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string string f))
  path)

(defun render-range-to-tex (path hand->lines)
  "Write a complete A4 LaTeX file at PATH from HAND->LINES.
   Example HAND->LINES (alist):
   '((\"AKo\" (\"CALL\" \"5\" \"SHOVE\")) (\"72o\" (\"FOLD\")))"
  (let* ((rows (build-table-rows hand->lines))
         (doc  (make-tex-document rows)))
    (write-string-to-file path doc)))


