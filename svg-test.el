(defvar svg-test-width 17.0 "Width of SVG blocks.")
(defvar svg-test-height 22.0 "Height of SVG blocks.")

(defmacro svg-test-defshape (name &rest body)
  (declare (indent 1))
  `(defun ,(intern (concat "svg-test-insert-" (symbol-name name))) ()
     (let ((svg (svg-create svg-test-width svg-test-height)))
       ,@body
       (insert-image (svg-image svg)))))

(svg-test-defshape commit
  (svg-line svg
	    (/ svg-test-width 2) 0
	    (/ svg-test-width 2) svg-test-height
	    :stroke-color 'white)
  (svg-circle svg
	      (/ svg-test-width 2)
	      (- (/ svg-test-height 2) 2)
	      (/ (1- svg-test-width) 2)
	      :stroke-color 'black :fill-color 'white))

(defvar svg-test-layer-1-items '(:n :ne :e :se :s :sw :w :nw))
(defvar svg-test-layer-2-items '(:commit))

(defvar svg-test-components (make-hash-table)  
  "List of pairs containing component and layer")

(defvar svg-test-draw-commands
  '((:line . svg-line)
    (:circle . svg-circle))
  "Supported simple draw commands and the actual corresponding function.")

(defvar svg-test-draw-shortcuts
  '((:middlex . (/ svg-test-width 2))
    (:middley . (/ svg-test-height 2)))
  "Supported shortcuts for use in defining components.")

(defun svg-test-draw-command (list svg-symbol)
  (let ((command (alist-get (car list) svg-test-draw-commands))
	(args (mapcar (lambda (sym) (alist-get sym svg-test-draw-shortcuts sym))
	       (cdr list))))
    (when (null command)
      (error "SVG draw command not found: %s" (car list)))
    `(,command ,svg-symbol ,@args)
    )
  )

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
  (svg-line svg
	    (/ svg-test-width 2) 0
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :ne 1
  (svg-line svg
	    svg-test-width 0
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :e 1
  (svg-line svg
	    svg-test-width (/ svg-test-height 2)
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :se 1
  (svg-line svg
	    svg-test-width svg-test-height
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :s 1
  (svg-line svg
	    (/ svg-test-width 2) svg-test-height
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :sw 1
  (svg-line svg
	    0 svg-test-height
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :w 1
  (svg-line svg
	    0 (/ svg-test-height 2)
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :nw 1
  (svg-line svg
	    0 0
	    (/ svg-test-width 2) (/ svg-test-height 2)
	    :stroke-color 'white))

(svg-test-defcomponent :commit 2
  (svg-circle svg
	      (/ svg-test-width 2)
	      (/ svg-test-height 2)
	      (/ svg-test-width 4)
	      :stroke-color 'black :fill-color 'white))

(svg-test-defcomponent :blank 0)

(defun svg-test-component-dispatch (svg symbol)
  (let* ((f-name (concat "svg-test-component-" (substring (symbol-name symbol) 1)))
	 (f (intern-soft f-name)))
    (when (null f)
      (error "No function %s" f-name))
    (funcall f svg)))

(defun svg-test-put-svg (&rest components)
  "Makes an SVG with the given components."
  (let ((svg (svg-create svg-test-width svg-test-height)))
    (mapc (lambda (symbol) (svg-test-component-dispatch svg symbol))
	  components)
    (insert-image (svg-image svg))))

(defun svg-test-show-all-components ()
  (with-current-buffer (get-buffer-create "*svg-test*")
    (erase-buffer)
    (maphash (lambda (key value)
	       (svg-test-put-svg key)
	       (insert (format " %s (layer %d)\n" key value)))
	     svg-test-components)
    (pop-to-buffer "*svg-test*")))

(defun svg-test-make-graph ()
  (with-current-buffer (get-buffer-create "*svg-test*")
    (erase-buffer)
    (svg-test-put-svg :s :commit) (insert " Commit\n")
    (svg-test-put-svg :n :s :commit) (insert " Commit\n")
    (svg-test-put-svg :n :s :se :commit) (insert " Commit\n")
    (svg-test-put-svg :n :s) (svg-test-put-svg :nw :s :commit) (insert " Commit\n")
    (svg-test-put-svg :n :commit) (svg-test-put-svg :n :s) (insert " Commit\n")
    (svg-test-put-svg :blank) (svg-test-put-svg :n :sw :commit) (insert " Commit\n")
    (svg-test-put-svg :ne :commit) (insert " Commit\n")
    (pop-to-buffer "*svg-test*")))
