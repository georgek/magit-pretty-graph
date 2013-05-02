;; (defvar magit-pg-command
;;   "git --no-pager log --pretty=format:\"%h %H %P\" -n 20")
(defvar magit-pg-command
  "git --no-pager log --topo-order --pretty=format:\"%H%x00%P%x00%h%x00%an%x00%ar%x00%s\" 5223017")

(defvar magit-graph-head "┍")
(defvar magit-graph-node "┝")
(defvar magit-graph-tail "┕")
(defvar magit-graph-down "│")
(defvar magit-graph-branchright "├")
(defvar magit-graph-branchleft "┤")
(defvar magit-graph-branchcross "┼")
(defvar magit-graph-branchdown "┬")
(defvar magit-graph-branchup "┴")
(defvar magit-graph-across "─")
(defvar magit-graph-topright "╮")
(defvar magit-graph-bottomright "╯")
(defvar magit-graph-topleft "╭")
(defvar magit-graph-bottomleft "╰")
(defvar magit-pg-buffer-name "*magit-prettier-graph*")
(defvar magit-pg-output-buffer-name "*magit-prettier-graph-output*")

(defun magit-pg-repo (directory)
  (with-current-buffer (get-buffer-create
                        magit-pg-output-buffer-name)
    (erase-buffer)
    (cd directory)
    (call-process-shell-command
     magit-pg-command
     nil
     magit-pg-output-buffer-name))
  (magit-pg magit-pg-output-buffer-name))

(defun magit-parse-hash (hash-str)
  (let (hash)
    (dotimes (i 4)
      (push (string-to-number
             (substring hash-str (* i 10) (+ (* i 10) 10)) 16)
            hash))
    hash))

(defun magit-pg-hash (commit)
  (cadr commit))

(defun magit-pg-parents (commit)
  (cddr commit))

(defun magit-pg-shorthash (commit)
  (first (car commit)))

(defun magit-pg-author (commit)
  (second (car commit)))

(defun magit-pg-date (commit)
  (third (car commit)))

(defun magit-pg-message (commit)
  (fourth (car commit)))

(defun magit-pg-commit-string (commit)
  (concat
   (magit-pg-shorthash commit)
   " ("
   (magit-pg-author commit)
   " - "
   (magit-pg-date commit)
   ") "
   (magit-pg-message commit)))

(defun magit-pg-parse-output (buffer)
  (with-current-buffer buffer
    (when (string= (substring (buffer-string) 0 5) "fatal")
      (error "Git error, see output buffer for details"))
    (let ((commits (mapcar
                    #'(lambda (line)
                        (split-string line "\0" nil))
                    (split-string (buffer-string) "\n" t))))
      (setq commits
            (mapcar
             #'(lambda (commit)
                 (setq commit (cons (cddr commit) commit))
                 (setcdr (cdr commit) (split-string (caddr commit) " " t))
                 commit)
             commits)))))

(defun magit-pg-print-commit (trunks commit)
  "Prints a commit node, returns new trunks, destroys trunks input."
  (when (null trunks) (setq trunks (list nil)))
  (let* ((head (not (member (magit-pg-hash commit) trunks)))
         (tail (and (not head) (null (magit-pg-parents commit)))))
    (when head
      (dolistc (trunkc trunks)
        (cond
         ((null (car trunkc))
          (setcar trunkc (magit-pg-hash commit))
          (return))
         ((null (cdr trunkc))
          (setcdr trunkc (cons (magit-pg-hash commit) nil))
          (return)))))
    (dolist (trunk trunks)
      (cond
       ((equal trunk (magit-pg-hash commit))
        (cond
         (head
          (insert magit-graph-head " "))
         (tail
          (insert magit-graph-tail " "))
         (t
          (insert magit-graph-node " "))))
       (trunk
        (insert magit-graph-down " "))
       (t
        (insert "  "))))
    (insert (magit-pg-commit-string commit) "\n")
    trunks))

(defun magit-pg-print-merge (trunks commit parents)
  "Prints a merge (if there is one) and returns new trunks (destructively)."
  (let (merge
        (trunk-merges (list)))
    (dolistc (trunkc trunks)
      (when (equal (car trunkc) (magit-pg-hash commit))
        (setcar trunkc (pop parents))
        (setq merge (car trunkc))
        (return)))

    (when (consp parents)
      ;; deal with merge
      (let ((new-parents (set-difference parents trunks :test #'equal))
            (last-parent)
            (first-parent))
        ;; fill in nils and find rightmost parent
        (dolistc (trunkc trunks)
          (cond
           ((eq (car trunkc) merge)
            (or first-parent (setq first-parent (car trunkc)))
            (setq last-parent (car trunkc)))
           ((and (null (car trunkc)) (consp new-parents))
            (setcar trunkc (pop new-parents))
            (or first-parent (setq first-parent (car trunkc)))
            (setq last-parent (car trunkc)))
           ((member (car trunkc) parents)
            (push (car trunkc) trunk-merges)
            (or first-parent (setq first-parent (car trunkc)))
            (setq last-parent (car trunkc)))))
        (when (consp new-parents)
          (setq last-parent (car (last new-parents)))
          (setq trunks (nconc trunks new-parents)))
        ;; draw merge
        (let ((str " ")
              (before-merge t))
          (dolist (trunk trunks)
            (cond
             ((eq first-parent trunk)
              (setq str magit-graph-across)
              (cond
               ((and before-merge (eq merge trunk))
                (setq before-merge nil)
                (insert magit-graph-branchright str))
               ((memq trunk trunk-merges)
                (insert magit-graph-down magit-graph-topleft))
               (t
                (insert magit-graph-topleft str))))

             ((eq last-parent trunk)
              (setq str " ")
              (cond
               ((and before-merge (eq merge trunk))
                (setq before-merge nil)
                (insert magit-graph-branchleft str))
               ((memq trunk trunk-merges)
                (delete-char -1)
                (insert magit-graph-topright magit-graph-down str))
               (t
                (insert magit-graph-topright str))))

             (t
              (cond
               ((and before-merge (eq merge trunk))
                (setq before-merge nil)
                (insert magit-graph-cross str))
               ((memq trunk trunk-merges)
                (if before-merge
                    (insert magit-graph-down magit-graph-branchdown)
                  (delete-char -1)
                  (insert magit-graph-branchdown magit-graph-down str)))
               (trunk
                (insert magit-graph-down str))
               (t
                (insert str str))))))
          (insert "\n")
          ;; draw rest of trunk merges
          (setq str " ")
          (setq before-merge t)
          (when (consp trunk-merges)
            (dolist (trunk trunks)
              (cond
               ((and before-merge (eq merge trunk))
                (setq before-merge nil)
                (insert magit-graph-down str))
               ((memq trunk trunk-merges)
                (if before-merge
                    (insert magit-graph-branchright magit-graph-bottomright)
                  (delete-char -1)
                  (insert magit-graph-bottomleft magit-graph-branchleft str)))
               (trunk
                (insert magit-graph-down str))
               (t
                (insert str str))))
            (insert "\n")))))

    trunks))

(defun magit-pg (buffer)
  ;; TODO parse hashes into lists of ints to save string comparisons (split
  ;; strings into 4 lengths of 10 chars to parse)
  (let ((commits (magit-pg-parse-output buffer))
        (trunks (list)))                ; this holds the hashes of the next
                                        ; node on each trunk (nil if trunk is
                                        ; unused)
    ;; print graph
    (with-current-buffer (get-buffer-create magit-pg-buffer-name)
      (setq mode-name "Magit Log")
      (erase-buffer)
      (toggle-truncate-lines 1)
      
      (dolist (commit commits)
        ;; print commit
        (setq trunks (magit-pg-print-commit trunks commit))
        ;; print merge and prepare parents
        (setq trunks (magit-pg-print-merge
                      trunks commit (magit-pg-parents commit)))
        ;; print branches
        (let ((l nil))
          (dolistc (trunkc trunks)
            ;; find last element with same hash
            (unless (null (car trunkc))
              (dolistc (otrunkc (rest trunkc))
                (when (equal (car trunkc) (car otrunkc))
                  (setq l otrunkc)
                  (setcar otrunkc 'same)))
              (when l                 ; branch
                (let ((str " "))
                  (dolistc (otrunkc trunks)
                    (cond
                     ((equal (car otrunkc) (car trunkc))
                      (setq str magit-graph-across)
                      (insert magit-graph-branchright str))
                     ((equal (car otrunkc) 'same)
                      (if (not (eq otrunkc l))
                          (insert magit-graph-branchup str)
                        (setq str " ")
                        (insert magit-graph-bottomright str))
                      (setcar otrunkc nil))
                     ((car otrunkc)
                      (insert magit-graph-down str))
                     (t
                      (insert str str)))))
                (insert "\n"))
              (setq l nil))))
        ;; remove nils at end
        (let ((last trunks))
          (dolistc (trunkc trunks)
            (when (car trunkc)
              (setq last trunkc)))
          (setcdr last nil))))))

(defmacro dolistc (spec &rest body)
  "Like dolist but VAR is bound to the cons instead of the car."
  (declare (indent 1))
  `(cl-block nil
     (let ((,(first spec) ,(second spec)))
       (while (consp ,(first spec))
         ,@body
         (setq ,(first spec) (rest ,(first spec)))))))
