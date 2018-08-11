;;; -*-coding: utf-8 -*-
;;; testing routines for pretty graph
(require 'cl-lib)

(defconst magit-pg-test-buffer-name "*magit-pg-test*")

(defvar magit-pg-tests (list)
  "All defined test functions.")

(defmacro magit-pg-deftest (name output &rest body)
  (declare (indent 2))
  (let ((vars (list))
        (test-sym (intern (concat "magit-pg-testfun-" (symbol-name name))))
        (output-str (mapconcat #'identity output "\n"))
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
             (string= (substring-no-properties (buffer-string)) ,output-sym))))
       (push ',test-sym magit-pg-tests)
       (setq magit-pg-tests (cl-remove-duplicates magit-pg-tests)))))

(defmacro magit-pg-runtest (test)
  `(funcall (function
             ,(intern (concat "magit-pg-testfun-" (symbol-name test))))))

(defun magit-pg-test-empty-commit (hash parents)
  "Makes a valid commit with given hash (an object) and
parents (a list)."
  (make-magit-pg-commit
   :hash hash
   :parent-hashes parents))

(magit-pg-deftest mergeleft ("├┬│─┬─╮  "
                             "│╰┤ │ │ ")
  (setq commit (magit-pg-test-empty-commit 1 (list 5 2 3 4)))
  (let ((magit-pg-trunks (list 1 2)))
   (insert
    (magit-pg-print-merge commit (magit-pg-commit-parent-hashes commit)))))

(magit-pg-deftest mergemid ("│╭┼╮│  "
                            "├╯│╰┤ ")
  (setq commit (magit-pg-test-empty-commit 2 (list 4 1 3)))
  (let ((magit-pg-trunks (list 1 2 3)))
   (insert
    (magit-pg-print-merge commit (magit-pg-commit-parent-hashes commit)))))

(magit-pg-deftest mergeright ("╭─┬─│┬┤  "
                              "│ │ ├╯│ ")
  (setq commit (magit-pg-test-empty-commit 4 (list 5 1 2 3)))
  (let ((magit-pg-trunks (list nil nil 3 4)))
   (insert
    (magit-pg-print-merge commit (magit-pg-commit-parent-hashes commit)))))

(magit-pg-deftest branchleft ("├─│─┴─│─┴─│─┴─│─╯ ")
  (let ((magit-pg-trunks (list 1 2 1 3 1 4 1 5 1)))
    (insert
     (magit-pg-print-branches))))

(magit-pg-deftest branchmulti ("├─┴─│─│─│─╯ │ │ │  "
                               "│   ├─┴─│───╯ │ │  "
                               "│   │   ├─────┴─╯ ")
  (let ((magit-pg-trunks (list 1 1 2 2 3 1 2 3 3)))
    (insert
     (magit-pg-print-branches))))

(defun magit-pg-test-alltests ()
  (cl-reduce #'(lambda (t1 t2) (and t1 t2))
          (mapcar #'funcall magit-pg-tests)))
