;; (defvar magit-pretty-graph-command
;;   "git --no-pager log --pretty=format:\"%h %H %P\" -n 20")
(defvar magit-pretty-graph-command
  "git --no-pager log --pretty=format:\"%H%x00%P%x00%h%x00%an%x00%ar%x00%s\" -n 20")

(defvar magit-graph-head "┍")
(defvar magit-graph-node "┝")
(defvar magit-graph-down "│")
(defvar magit-graph-branch "├")
(defvar magit-graph-branchin "┤")
(defvar magit-graph-branchdown "┬")
(defvar magit-graph-branchup "┴")
(defvar magit-graph-across "─")
(defvar magit-graph-out "╮")
(defvar magit-graph-in "╯")
(defvar magit-graph-trunkmerge "╰")
(defvar magit-prettier-graph-buffer-name "*magit-prettier-graph*")
(defvar magit-prettier-graph-output-buffer-name "*magit-prettier-graph-output*")

(defun magit-prettier-graph-repo (directory)
  (with-current-buffer (get-buffer-create
                        magit-prettier-graph-output-buffer-name)
    (erase-buffer)
    (cd directory)
    (call-process-shell-command
     magit-pretty-graph-command
     nil
     magit-prettier-graph-output-buffer-name))
  (magit-prettier-graph magit-prettier-graph-output-buffer-name))

(defun magit-parse-hash (hash-str)
  (let (hash)
    (dotimes (i 4)
      (push (string-to-number
             (substring hash-str (* i 10) (+ (* i 10) 10)) 16)
            hash))
    hash))

(defun magit-prettier-graph-hash (commit)
  (cadr commit))

(defun magit-prettier-graph-parents (commit)
  (cddr commit))

(defun magit-prettier-graph-shorthash (commit)
  (first (car commit)))

(defun magit-prettier-graph-author (commit)
  (second (car commit)))

(defun magit-prettier-graph-date (commit)
  (third (car commit)))

(defun magit-prettier-graph-message (commit)
  (fourth (car commit)))

(defun magit-prettier-graph-commit-string (commit)
  (concat
   (magit-prettier-graph-shorthash commit)
   " ("
   (magit-prettier-graph-author commit)
   " - "
   (magit-prettier-graph-date commit)
   ") "
   (magit-prettier-graph-message commit)))

(defun magit-prettier-graph-parse-output (buffer)
  (with-current-buffer buffer
    (let ((commits (mapcar
                    #'(lambda (line)
                        (split-string line "\0" t))
                    (split-string (buffer-string) "\n" t))))
      (setq commits
            (mapcar
             #'(lambda (commit)
                 (setq commit (cons (cddr commit) commit))
                 (setcdr (cdr commit) (split-string (caddr commit) " " t))
                 commit)
             commits)))))

(defun magit-prettier-graph (buffer)
  ;; TODO parse hashes into lists of ints to save string comparisons (split
  ;; strings into 4 lengths of 10 chars to parse)
  (let ((commits (magit-prettier-graph-parse-output buffer))
        (trunks (list)))                ; this holds the hashes of the next
                                        ; node on each trunk (nil if trunk is
                                        ; unused)
    ;; print graph
    (let ((this-trunk nil)
          (head nil))
      (with-current-buffer (get-buffer-create magit-prettier-graph-buffer-name)
        (setq mode-name "Magit Log")
        (erase-buffer)
        (toggle-truncate-lines 1)
       
        (dolist (commit commits)
          (setq this-trunk (position 
                            (magit-prettier-graph-hash commit)
                            trunks :test #'equal))
          (setq head nil)
          (when (not this-trunk)
            ;; new head
            (setq head t)
            (setq this-trunk (or (position nil trunks) (length trunks)))
            (when (>= this-trunk (length trunks))
              (setq trunks (append trunks 
                                   (list (magit-prettier-graph-hash commit))))))
          ;; print trunks
          (dolist (trunk trunks)
            (cond
             ((equal trunk (magit-prettier-graph-hash commit))
              (if head
                  (insert magit-graph-head " ")
                (insert magit-graph-node " ")))
             (trunk
              (insert magit-graph-down " "))
             (t
              (insert "  "))))
          (insert (magit-prettier-graph-commit-string commit) "\n")
          ;; prepare parents
          (let ((parents (magit-prettier-graph-parents commit))
                (trunk-branches (list)))
            (setf (nth this-trunk trunks) (first parents))
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
                    (l (car (last parents)))
                    (f nil))
                (dolist (trunk trunks)
                  (cond
                   ((equal trunk (first (magit-prettier-graph-parents commit)))
                    ;; only merge into first trunk, any others are branches
                    ;; which will be dealt with next
                    (if f
                        (insert magit-graph-down str)
                      (setq f t)
                      (setq str magit-graph-across)
                      (insert magit-graph-branch str)))
                   (trunk
                    (if (member trunk parents)
                        (if (not (equal trunk l))
                            (if (not (member trunk trunk-branches))
                                (insert magit-graph-branchdown str)
                              (delete-char -1)
                              (insert magit-graph-branchdown
                                      magit-graph-down str))
                          (setq str " ")
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
                        (insert magit-graph-branch str))
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
            (setcdr last nil)))))))

(defmacro dolistc (spec &rest body)
  "Like dolist but VAR is bound to the cons instead of the car."
  (declare (indent 1))
  `(cl-block nil
     (let ((,(first spec) ,(second spec)))
       (while (consp ,(first spec))
         ,@body
         (setq ,(first spec) (rest ,(first spec)))))))

(magit-prettier-graph "testlog2")
