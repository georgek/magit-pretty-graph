;;; testing routines for pretty graph
(require 'cl-lib)

(defconst magit-pg-test-buffer-name "*magit-pg-test*")

(defvar magit-pg-tests (list)
  "All defined test functions.")

(defmacro magit-pg-deftest (name output &rest body)
  (declare (indent 2))
  (let ((vars (list))
        (test-sym (intern (concat "magit-pg-testfun-" (symbol-name name))))
        (output-str (apply #'concat
                           (mapcar #'(lambda (s) (concat s "\n"))
                                   output)))
        (output-sym (gensym)))
    (mapc #'(lambda (form) (when (eq (car form) 'setq) (push (cadr form) vars)))
          body)
    (setq vars (cl-remove-duplicates vars :test #'eq))
    `(progn
       (defun ,test-sym ()
         (let (,@vars
               (,output-sym ,output-str))
           (with-current-buffer (get-buffer-create magit-pg-test-buffer-name)
             (setq mode-name "Magit Log Test")
             (erase-buffer)
             (toggle-truncate-lines 1)
             ,@body
             (string= (buffer-string) ,output-sym))))
       (push ',test-sym magit-pg-tests)
       (setq magit-pg-tests (cl-remove-duplicates magit-pg-tests)))))

(defmacro magit-pg-runtest (test)
  `(funcall (function
             ,(intern (concat "magit-pg-testfun-" (symbol-name test))))))

(defun magit-pg-test-empty-commit (hash parents)
  "Makes a valid commit with given hash (an object) and
parents (a list)."
  (nconc (list (list "" "" "" "")
               hash)
         parents))

(magit-pg-deftest mergeleft ("├┬│─┬─╮ "
                             "│╰┤ │ │ ")
  (setq trunks (list 1 2))
  (setq commit (magit-pg-test-empty-commit 1 (list 5 2 3 4)))
  (magit-pg-print-merge trunks commit (magit-pg-parents commit)))

(magit-pg-deftest mergemid ("│╭┼╮│ "
                            "├╯│╰┤ ")
  (setq trunks (list 1 2 3))
  (setq commit (magit-pg-test-empty-commit 2 (list 4 1 3)))
  (magit-pg-print-merge trunks commit (magit-pg-parents commit)))

(magit-pg-deftest mergeright ("╭─┬─│┬┤ "
                              "│ │ ├╯│ ")
  (setq trunks (list nil nil 3 4))
  (setq commit (magit-pg-test-empty-commit 4 (list 5 1 2 3)))
  (magit-pg-print-merge trunks commit (magit-pg-parents commit)))

(magit-pg-deftest branchleft ("├─│─┴─│─┴─│─┴─│─╯ ")
  (setq trunks (list 1 2 1 3 1 4 1 5 1))
  (magit-pg-print-branches trunks))

(magit-pg-deftest branchmulti ("├─┴─│─│─│─╯ │ │ │ "
                               "│   ├─┴─│───╯ │ │ "
                               "│   │   ├─────┴─╯ ")
  (setq trunks (list 1 1 2 2 3 1 2 3 3))
  (magit-pg-print-branches trunks))

(defun magit-pg-test-alltests ()
  (reduce #'(lambda (t1 t2) (and t1 t2))
          (mapcar #'funcall magit-pg-tests)))

