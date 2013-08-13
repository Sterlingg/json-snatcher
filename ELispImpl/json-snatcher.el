;; json-snatcher.el
;; Author: Sterling Graham
(defun get-str-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((return_vector (vector)) (string_regex "\"\\(\\([^\"\\\\[:space:]]\\)*\\([:space:]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^[:space:]]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\""))
      (while (re-search-forward string_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(setq return_vector (vconcat return_vector (vector (buffer-substring (match-beginning 0) (match-end 0))))))
      return_vector
      )))

(defun get-num-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((return_vector (vector)) (num_regex "-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?"))
      (while (re-search-forward num_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(setq return_vector (vconcat return_vector (vector (buffer-substring (match-beginning 0) (match-end 0))))))
      return_vector
      )))

(defun get-delim-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((return_vector (vector)) (delim_regex "\\([\][\\{\\}:,]\\)"))
      (while (re-search-forward delim_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(setq return_vector (vconcat return_vector (vector (buffer-substring (match-beginning 0) (match-end 0))))))
      return_vector
      )))

(defun parse ()
  ""
  (interactive)
  (let (var1)
    (setq var1 some)
    
    ))

(defun get-json-tokens ()
  "Get all tokens necessary for the parser."
  (interactive)
  (let (var1)
    (setq var1 some)
    
    ))
(get-str-tokens "~/Programming/Projects/json-snatcher/el-impl/Test/test_strings.in")
(get-str-tokens "~/Programming/Projects/json-snatcher/el-impl/Test/aws.json")
(get-num-tokens "~/Programming/Projects/json-snatcher/el-impl/Test/test_num.in")
(get-delim-tokens "~/Programming/Projects/json-snatcher/el-impl/Test/rfc_object.json")
