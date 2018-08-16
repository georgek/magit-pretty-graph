;;; svg-test.el --- Testing drawing git graphs with SVG -*- lexical-binding: t -*-

;; Author: George Kettleborough
;; Maintainer: George Kettleborough
;; Version: 0.0.1
;; Package-Requires: (dash)
;; Homepage: https://github.com/georgek/magit-pretty-graph
;; Keywords: magit, git


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Run `svg-test-repo'

;;; Code:
(require 'svg)
(require 'magit-pretty-graph)

(defvar svg-test-debug nil)

(defvar svg-test-width 17.0 "Width of SVG blocks")
(defvar svg-test-height 22.0 "Height of SVG blocks")

(defvar svg-test-components (make-hash-table)  
  "List of pairs containing component and layer")

(defvar svg-test-draw-commands
  '((:line . svg-line)
    (:circle . svg-circle))
  "Supported simple draw commands and the actual corresponding function")

(defvar svg-test-draw-shortcuts
  '((:xmid . (/ svg-test-width 2))
    (:ymid . (/ svg-test-height 2))
    (:top . 0)
    (:bottom . svg-test-height)
    (:left . 0)
    (:right . svg-test-width))
  "Supported shortcuts for use in defining components")

(defun svg-test-draw-command (list svg-symbol)
  (let ((command (alist-get (car list) svg-test-draw-commands))
	(args (mapcar (lambda (sym) (alist-get sym svg-test-draw-shortcuts sym))
	       (cdr list))))
    (when (null command)
      (error "SVG draw command not found: %s" (car list)))
    `(,command ,svg-symbol ,@args)))

(defmacro svg-test-defcomponent (name layer &rest body)
  (declare (indent 2))
  (let ((component-symbol
	 (intern (concat "svg-test-component-"
			 (substring (symbol-name name) 1))))
	(svg-symbol (gensym)))
    `(progn
       (puthash ',name ,layer svg-test-components)
       (defun ,component-symbol (,svg-symbol)
	 ,@(mapcar (lambda (command) (svg-test-draw-command command svg-symbol))
		   body)))))

(svg-test-defcomponent :n 1
  (:line :xmid :top
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :ne 1
  (:line :right :top
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :e 1
  (:line :right :ymid
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :se 1
  (:line :right :bottom
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :s 1
  (:line :xmid :bottom
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :sw 1
  (:line :left :bottom
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :w 1
  (:line :left :ymid
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :nw 1
  (:line :left :top
	 :xmid :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :top-to-left 1
  (:line :xmid :top
	 :left :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :across 1
  (:line :left :ymid
	 :right :ymid
	 :stroke-color 'white))

(svg-test-defcomponent :commit 2
  (:circle :xmid
	   :ymid
	   (/ svg-test-width 4)
	   :stroke-color 'black :fill-color 'white))

(svg-test-defcomponent :blank 0)

(defun svg-test-component-dispatch (svg symbol)
  (when (null symbol)
    (setq symbol :blank))
  (let* ((f-name (concat "svg-test-component-" (substring (symbol-name symbol) 1)))
	 (f (intern-soft f-name)))
    (when (null f)
      (error "No function %s" f-name))
    (funcall f svg)))

(defun svg-test-add-to-svg (svg &rest components)
  "Add components to given SVG"
  (mapc (lambda (symbol) (svg-test-component-dispatch svg symbol))
	components)
  svg)

(defun svg-test-make-svg (&rest components)
  "Makes an SVG with the given components"
  (let ((svg (svg-create svg-test-width svg-test-height :stroke-width 1)))
    (apply #'svg-test-add-to-svg svg components)))

(defun svg-test-put-svg (&rest components)
  "Makes an SVG and inserts it to buffer"
  (insert-image (svg-image (apply #'svg-test-make-svg components))))

(defun svg-test-show-all-components ()
  (with-current-buffer (get-buffer-create "*svg-test*")
    (erase-buffer)
    (maphash (lambda (key value)
	       (svg-test-put-svg key)
	       (insert (format " %s (layer %d)\n" key value)))
	     svg-test-components)
    (pop-to-buffer "*svg-test*")))

(defun svg-test-make-graph ()
  (svg-test-put-svg :s :commit) (insert " One\n")
  (svg-test-put-svg :n :s :commit) (insert " Two\n")
  (svg-test-put-svg :n :s :se :commit) (insert " Three\n")
  (svg-test-put-svg :n :s) (svg-test-put-svg :nw :s :commit) (insert " Four\n")
  (svg-test-put-svg :n :commit) (svg-test-put-svg :n :s) (insert " Five\n")
  (svg-test-put-svg :blank) (svg-test-put-svg :n :sw :commit) (insert " Six\n")
  (svg-test-put-svg :ne :commit) (insert " Seven\n"))

(cl-defstruct svg-test-commit-line
  commit incoming-hashes outgoing-hashes svgs)

(defun svg-test-incoming-to-outgoing-hashes (incoming-hashes commit)
  (let ((commit-hash (magit-pg-commit-hash commit))
	(parent-hashes (magit-pg-commit-parent-hashes commit)))
    (-distinct (--splice-list (equal it commit-hash)
			      parent-hashes
			      incoming-hashes))))

(defun svg-test-make-commit-line (commit incoming-hashes)
  "Make the line for this commit in the context of the incoming
hashes. The incoming hashes are the hashes of all the parents of
previous commits that we are still waiting for"
  (let* ((commit-hash (magit-pg-commit-hash commit))
	 (commit-index (-elem-index commit-hash incoming-hashes)))
    (unless commit-index
      ;; we weren't waiting for this commit, so it must be the head of a
      ;; branch we haven't seen yet
      (push commit-hash incoming-hashes)
      (setq commit-index 0))
    (let ((outgoing-hashes (svg-test-incoming-to-outgoing-hashes
			    incoming-hashes commit)))
     (make-svg-test-commit-line
      :commit commit
      :incoming-hashes incoming-hashes
      :outgoing-hashes outgoing-hashes
      :svgs (--map (svg-test-make-svg :blank) incoming-hashes)))))

(defun svg-test-format-hashes (hashes)
  (mapconcat #'magit-pg-format-hash hashes ", "))

(defun svg-test-apply-edge (svgs n-outgoing-edges i j)
  "Draws edge in this row to connect column i to column j (as a
side effect) and returns a list of edges to draw on the next
commit line"
  ;; FIXME: this will break if difference between i and j is greater than 1
  ;; (ie. cause by octopus merges, new heads and far away branches)
  (let ((svg (nth i svgs))
	(outgoing-directions (make-list n-outgoing-edges :blank)))
   (cond ((= i j)
	  (svg-test-add-to-svg svg :s)
	  (setf (nth j outgoing-directions) :n))
	 ((= (- i j) 1)
	  (svg-test-add-to-svg svg :sw)
	  (setf (nth j outgoing-directions) :ne))
	 ((= (- i j) -1)
	  (svg-test-add-to-svg svg :se)
	  (setf (nth j outgoing-directions) :nw))
	 ((> (- i j) 1)
	  (svg-test-add-to-svg svg :s)
	  (setf (nth i outgoing-directions) :top-to-left)
	  (cl-loop
	   for n from (1+ j) below i do
	   (setf (nth n outgoing-directions) :across))
	  (setf (nth j outgoing-directions) :e))
	 (t :blank))
   outgoing-directions))

(defun svg-test-fill-in-commit-line (commit-line incoming-directions)
  "Fills in the graph for the SVGs in the commit line (as a side
  effect). The incoming directions list the directions the
  incoming edges are coming from. Returns the incoming directions
  for the next line."
  (let* ((commit (svg-test-commit-line-commit commit-line))
	 (incoming-hashes (svg-test-commit-line-incoming-hashes commit-line))
	 (outgoing-hashes (svg-test-commit-line-outgoing-hashes commit-line))
	 (commit-hash (magit-pg-commit-hash commit))
	 (commit-index (-elem-index commit-hash incoming-hashes))
	 (svgs (svg-test-commit-line-svgs commit-line))
	 (outgoing-directions (make-list (length outgoing-hashes) (list))))
    ;; draw incoming edges
    (cl-loop
     for incoming-direction in incoming-directions
     for svg in svgs do
     (apply #'svg-test-add-to-svg svg incoming-direction))
    ;; draw outgoing edges
    (cl-loop
     for incoming-hash in incoming-hashes
     for i from 0 do
     (if (equal incoming-hash commit-hash)
	 ;; connect commit to parents
	 (let ((parent-hashes (magit-pg-commit-parent-hashes commit)))
	   (cl-loop
	    for parent-hash in parent-hashes do
	    (let* ((j (-elem-index parent-hash outgoing-hashes))
		   (new-outgoing-directions (svg-test-apply-edge svgs (length outgoing-hashes) i j)))
	      (setq outgoing-directions
		    (-zip-with #'cons new-outgoing-directions outgoing-directions)))))
       ;; connect other trunks
       (let ((j (-elem-index incoming-hash outgoing-hashes)))
	 (when j
	   (let ((new-outgoing-directions (svg-test-apply-edge svgs (length outgoing-hashes) i j)))
	     (setq outgoing-directions
		   (-zip-with #'cons new-outgoing-directions outgoing-directions)))))))
    ;; draw commit
    (svg-test-add-to-svg (nth commit-index svgs) :commit)
    outgoing-directions))

(defun svg-test-draw-commit-line (commit-line)
  (dolist (svg (svg-test-commit-line-svgs commit-line))
    (insert-image (svg-image svg)))
  (let ((commit (svg-test-commit-line-commit commit-line)))
    (insert
     (if svg-test-debug
	 (format " %s (p: %s) (i: %s) (o: %s)"
		 (or (magit-pg-commit-short-hash commit) (magit-pg-commit-hash commit))
		 (svg-test-format-hashes (magit-pg-commit-parent-hashes commit))
		 (svg-test-format-hashes (svg-test-commit-line-incoming-hashes commit-line))
		 (svg-test-format-hashes (svg-test-commit-line-outgoing-hashes commit-line)))
       (format " %s" (magit-pg-commit-string commit))))))

(defun svg-test-draw-repo (commits)
  (with-current-buffer (get-buffer-create "*svg-test*")
    (erase-buffer)
    (setq truncate-lines t)
    (let ((current-hashes (list))
	  (commit-lines (list)))
      (dolist (commit commits)
	(push (svg-test-make-commit-line commit current-hashes) commit-lines)
	;; FIXME: the incoming edges of the new commit might no longer
	;; match the outgoing edges of the previous commit line. This
	;; happens in the case of a new head or an octopus merge. If this
	;; happens the change needs to propagated back up the stack
	(setq current-hashes (svg-test-commit-line-outgoing-hashes (car commit-lines))))
      (setq commit-lines (nreverse commit-lines))
      (let ((current-directions (list)))
	(dolist (commit-line commit-lines)
	  (setq current-directions
		(svg-test-fill-in-commit-line commit-line current-directions))))
      (dolist (commit-line commit-lines)
	(svg-test-draw-commit-line commit-line)
	(insert "\n")))
    (insert "\n")
    (pop-to-buffer "*svg-test*")
    (beginning-of-buffer)
    (local-set-key (kbd "g") (lambda ()
			       (interactive)
			       (svg-test-draw-repo commits)))))

(defun svg-test-test ()
  (interactive)
  (let ((commits
	 (list
	  (make-magit-pg-commit :hash 1 :description "One" :parent-hashes '(2))
	  (make-magit-pg-commit :hash 2 :description "Two" :parent-hashes '(3))
	  (make-magit-pg-commit :hash 3 :description "Three" :parent-hashes '(5 4))
	  (make-magit-pg-commit :hash 4 :description "Four" :parent-hashes '(6))
	  (make-magit-pg-commit :hash 5 :description "Five" :parent-hashes '())
	  (make-magit-pg-commit :hash 6 :description "Six" :parent-hashes '(7))
	  (make-magit-pg-commit :hash 7 :description "Seven" :parent-hashes '()))))
    (svg-test-draw-repo commits)))

(defun svg-test-repo (&optional directory)
  (interactive
   (list (read-directory-name "Repository: ")))
  (with-current-buffer (get-buffer-create magit-pg-output-buffer-name)
    (erase-buffer)
    (cd (or directory default-directory))
    (call-process-shell-command
     magit-pg-command
     nil
     magit-pg-output-buffer-name)
    (let ((commits (magit-pg-parse-output magit-pg-output-buffer-name)))
      (svg-test-draw-repo commits))))

(provide 'svg-test)

;;; svg-test.el ends here
