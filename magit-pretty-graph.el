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
(defvar magit-graph-out "╮")
(defvar magit-graph-in "╯")
(defvar magit-graph-trunkmerge "╰")
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
        ;; prepare parents
        (let ((parents (magit-pg-parents commit))
              (trunk-branches (list)))
          (setf (nth
                 (position (magit-pg-hash commit) trunks :test #'equal)
                 trunks) (first parents))
          (setq parents (rest parents))
          (when (consp parents)
            ;; merge
            (let ((p parents))
              (dolistc (trunkc trunks)
                (while (and (consp p)
                            (member (first p) trunks))
                  (push (first p) trunk-branches)
                  (setq p (rest p)))
                (when (null p)
                  (return))
                (when (null (car trunkc))
                  (setcar trunkc (first p))
                  (setq p (rest p))))
              (when (consp p)
                (setq trunks (append trunks p))))
            (let ((str " ")
                  (f t)
                  (l (car (last parents)))
                  (b nil))
              (dolist (trunk trunks)
                (cond
                 ((equal trunk (first (magit-pg-parents commit)))
                  ;; only merge into first trunk, any others are branches
                  ;; which will be dealt with next
                  (if b
                      (insert magit-graph-down str)
                    (setq b t)
                    (if (null l)      ; no more branches to the right
                        (insert magit-graph-down str)
                      (setq str magit-graph-across)
                      (insert magit-graph-branch str))))
                 (trunk
                  (if (member trunk parents)
                      (if (not (equal trunk l))
                          (if (not (member trunk trunk-branches))
                              (insert magit-graph-branchdown str)
                            (delete-char -1)
                            (insert magit-graph-branchdown
                                    magit-graph-down str))
                        (setq str " ")
                        (setq l nil)  ; record that last one has been done
                        (if (not (member trunk trunk-branches))
                            (insert magit-graph-out str)
                          (delete-char -1)
                          (insert magit-graph-out
                                  magit-graph-down str)))
                    (insert magit-graph-down str)))
                 (t
                  (insert str str)))))
            (insert "\n")
            (when (consp trunk-branches)
              (dolist (trunk trunks)
                (cond
                 ((member trunk trunk-branches)
                  (delete-char -1)
                  (insert magit-graph-trunkmerge magit-graph-branchin " "))
                 (trunk
                  (insert magit-graph-down " "))
                 (t
                  (insert "  "))))
              (insert "\n"))))
        ;; find branches
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
                        (insert magit-graph-in str))
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
