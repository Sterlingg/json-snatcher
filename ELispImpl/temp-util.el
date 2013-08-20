(defun jsons-get-str-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    ;; TODO: Improve this regex. Although now it SEEMS to be working, and can be
    ;; used to validate escapes if needed later. The second half of it is pretty
    ;; pointless at the moment. I did it this way, so that the code closely mirrors
    ;; the RFC.
    (let ((return_list ()) (string_regex "\\(\"\\(\\([^\"\\\\[:space:]]\\)*\\([:space:]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^[:space:]]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)"))
      (while (re-search-forward string_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(push (buffer-substring (match-beginning 0) (match-end 0)) return_list))
      (reverse return_list))))

(defun jsons-get-num-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((return_list ()) (num_regex "-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?"))
      (while (re-search-forward num_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(push (buffer-substring (match-beginning 0) (match-end 0)) return_list))
      (reverse return_list))))

(defun jsons-get-delim-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((return_vector (vector)) (delim_regex "\\([\][\\{\\}:,]\\)"))
      (while (re-search-forward delim_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(setq return_vector (vconcat return_vector (vector (buffer-substring (match-beginning 0) (match-end 0))))))
      return_vector)))



;; (defun jsons-get-tokens (file)
;;   "Retrieves all of the strings in the current buffer."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (goto-char (point-min))
;;     (let* ((return_list ()) 
;; 	   (delim_regex "\\([\][\\{\\}:,]\\)")
;; 	   ;; TODO: Improve this regex. Although now it SEEMS to be working, and can be
;; 	   ;; used to validate escapes if needed later. The second half of the string regex it is pretty
;; 	   ;; pointless at the moment. I did it this way, so that the code closely mirrors
;; 	   ;; the RFC.
;; 	   (string_regex "\\(\"\\(\\([^\"\\\\\r\s\t\n]\\)*\\([\r\s\t\n]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^\r\s\t\n]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)")
;; 	   (num_regex "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)")
;; 	   (full_regex (concat "\\(" delim_regex "\\|" string_regex "\\|" num_regex "\\)")))
;;       (while (re-search-forward full_regex (point-max) "Not nil")
;; 	(goto-char (match-end 0))
;; 	(push (buffer-substring (match-beginning 0) (match-end 0)) return_list))
;;       (reverse return_list))))

;; (defun jsons-get-tokens (file)
;;   "Gets the next token from the given file."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (goto-char jsons-curr-token)
;;     (let* ((return_list ()) 
;; 	   (delim_regex "\\([\][\\{\\}:,]\\)")
;; 	   ;; TODO: Improve this regex. Although now it SEEMS to be working, and can be
;; 	   ;; used to validate escapes if needed later. The second half of the string regex it is pretty
;; 	   ;; pointless at the moment. I did it this way, so that the code closely mirrors
;; 	   ;; the RFC.
;; 	   (string_regex "\\(\"\\(\\([^\"\\\\\r\s\t\n]\\)*\\([\r\s\t\n]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^\r\s\t\n]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)")
;; 	   (num_regex "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)")
;; 	   (full_regex (concat "\\(" delim_regex "\\|" string_regex "\\|" num_regex "\\)")))
;;       (while (re-search-forward full_regex (point-max) "Not nil")
;; 	(goto-char (match-end 0))
;; 	(push (buffer-substring (match-beginning 0) (match-end 0)) return_list)
;; 	(setq jsons-curr-token (match-end 0))
;; 	)
;;       (reverse return_list))))
