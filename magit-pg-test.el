;;; testing routines for pretty graph

(defconst magit-pg-test-buffer-name "*magit-pg-test*")

(defun magit-pg-test-empty-commit (hash parents)
  "Makes a valid commit with given hash (an object) and
parents (a list)."
  (nconc (list (list "" "" "" "")
               hash)
         parents))

(defun magit-pg-test-merge ()
  (let (trunks
        commit
        (result
         (concat"├┬│─┬─╮ \n"
                "│╰┤ │ │ \n"
                "\n"
                "│╭┼╮│ \n"
                "├╯│╰┤ \n"
                "\n"
                "╭─┬─│┬┤ \n"
                "│ │ ├╯│ \n"
                "\n")))
    (with-current-buffer (get-buffer-create magit-pg-test-buffer-name)
      (setq mode-name "Magit Log Test")
      (erase-buffer)
      (toggle-truncate-lines 1)

      (setq trunks (list 1 2))
      (setq commit (magit-pg-test-empty-commit 1 (list 5 2 3 4)))
      (magit-pg-print-merge trunks commit (magit-pg-parents commit))
      (newline)
      
      (setq trunks (list 1 2 3))
      (setq commit (magit-pg-test-empty-commit 2 (list 4 1 3)))
      (magit-pg-print-merge trunks commit (magit-pg-parents commit))
      (newline)
      
      (setq trunks (list nil nil 3 4))
      (setq commit (magit-pg-test-empty-commit 4 (list 5 1 2 3)))
      (magit-pg-print-merge trunks commit (magit-pg-parents commit))
      (newline)
      
      (string= (buffer-string) result))))
