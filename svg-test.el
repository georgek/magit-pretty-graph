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

(svg-test-defcomponent :commit 2
  (:circle :xmid
	   :ymid
	   (/ svg-test-width 4)
	   :stroke-color 'black :fill-color 'white))

(svg-test-defcomponent :blank 0)

(defun svg-test-component-dispatch (svg symbol)
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
  (let ((svg (svg-create svg-test-width svg-test-height)))
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
  commit trunk-hashes svgs)

(defun svg-test-make-commit-line (commit trunk-hashes)
  "Makes the commits (nodes) with no edges"
  (let ((commit-line (make-svg-test-commit-line
		      :commit commit
		      :trunk-hashes (-copy trunk-hashes)
		      :svgs (--map (svg-test-make-svg :blank) trunk-hashes)))
	(commit-trunk (-elem-index (magit-pg-commit-hash commit) trunk-hashes)))
    (unless commit-trunk
      ;; new head
      (push (svg-test-make-svg :blank) (svg-test-commit-line-svgs commit-line))
      (push (magit-pg-commit-hash commit) (svg-test-commit-line-trunk-hashes commit-line))
      (setq commit-trunk 0))
    ;; mark this commit
    (svg-test-add-to-svg (nth commit-trunk (svg-test-commit-line-svgs commit-line))
			 :commit)
    ;; update trunks
    (setf (svg-test-commit-line-trunk-hashes commit-line)
	  (--splice (eq it (magit-pg-commit-hash commit))
		    (magit-pg-commit-parent-hashes commit)
		    (svg-test-commit-line-trunk-hashes commit-line)))
    (setf (svg-test-commit-line-trunk-hashes commit-line)
	  (-distinct (svg-test-commit-line-trunk-hashes commit-line)))
    commit-line))

(defun svg-test-draw-commit-line (commit-line)
  (dolist (svg (svg-test-commit-line-svgs commit-line))
    (insert-image (svg-image svg)))
  (let ((commit (svg-test-commit-line-commit commit-line)))
    (insert
     (format " %s %s"
	     (or (magit-pg-commit-short-hash commit) (magit-pg-commit-hash commit))
	     (magit-pg-commit-description commit)))))

(defun svg-test-repo ()
  (let ((commits
	 (list
	  (make-magit-pg-commit :hash 1 :description "One" :parent-hashes '(2))
	  (make-magit-pg-commit :hash 2 :description "Two" :parent-hashes '(3))
	  (make-magit-pg-commit :hash 3 :description "Three" :parent-hashes '(4 5))
	  (make-magit-pg-commit :hash 4 :description "Four" :parent-hashes '(6))
	  (make-magit-pg-commit :hash 5 :description "Five" :parent-hashes '())
	  (make-magit-pg-commit :hash 6 :description "Six" :parent-hashes '(7))
	  (make-magit-pg-commit :hash 7 :description "Seven" :parent-hashes '()))))
    (with-current-buffer (get-buffer-create "*svg-test*")
      (erase-buffer)
      (svg-test-make-graph)
      (insert "\n\n")
      (setq truncate-lines t)
      (let ((trunk-hashes (list))
	    (commit-lines (list)))
	(dolist (commit commits)
	    (push (svg-test-make-commit-line commit trunk-hashes) commit-lines)
	    (setq trunk-hashes (svg-test-commit-line-trunk-hashes (car commit-lines))))
	(setq commit-lines (nreverse commit-lines))
	(dolist (commit-line commit-lines)
	  (svg-test-draw-commit-line commit-line)
	  (insert "\n")))
      (insert "\n")
      (pop-to-buffer "*svg-test*")
      (end-of-buffer))))

(provide 'svg-test)

;;; svg-test.el ends here
