;; json-snatcher.el
;; Author: Sterling Graham

;; Working on: -Improve the performance of tokenizing by using lists instead of
;;             vectors in the tokenizing functions.

;;            - Finish up the parsing functions. 
;;            - Testing

(setq jsons-curr-token 0)
(setq jsons-tokens ())

(defun jsons-get-str-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    ;; TODO: Improve this regex. Although now it SEEMS to be working, and can be
    ;; used to validate escapes if needed later. The second half of it is pretty
    ;; pointless at the moment. I did it this way, so that the code closely mirrors
    ;; the RFC.
    (let ((return_list ()) (string_regex "\\(\"\\(\\([^\"\\\\[:space:]]\\)*\\([:space:]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^[:space:]]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)"))
      (while (re-search-backward string_regex (point-min) "Not nil")
	(goto-char (match-beginning 0))
	(push (buffer-substring (match-beginning 0) (match-end 0)) return_list))
      return_list)))

(defun jsons-get-num-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    (let ((return_list ()) (num_regex "-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?"))
      (while (re-search-backward num_regex (point-min) "Not nil")
	(goto-char (match-beginning 0))
	(push (buffer-substring (match-beginning 0) (match-end 0)) return_list))
      return_list)))

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

(defun jsons-get-json-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let* ((return_vector (vector)) 
	   (delim_regex "\\([\][\\{\\}:,]\\)")
	   (string_regex "\\(\"\\(\\([^\"\\\\[:space:]]\\)*\\([:space:]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^[:space:]]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)")
	   (num_regex "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)")
	   (full_regex (concat "\\(" delim_regex "\\|" string_regex "\\|" num_regex "\\)")))
      (while (re-search-forward full_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(setq return_vector (vconcat return_vector (vector (buffer-substring (match-beginning 0) (match-end 0))))))
      return_vector)))

(defun jsons-consume-token ()
  "Advances the parser to the next token."
    (if (< jsons-curr-token (length jsons-tokens))
	(prog1 
	  (elt jsons-tokens jsons-curr-token)
	  (setq jsons-curr-token (+ jsons-curr-token 1))
	  ) 
      nil))

(defun jsons-array ()
  "Function called when a [ is encountered."
  (let (token (jsons-consume-token)
	(array ["json-array" []]))
    (while (not (string= token "}"))
      (if (string= token ",")
	  (setq token (jsons-consume-token))
	(let (json-mem (jsons-member))
	  (puthash (elt json-mem 1) (elt json-mem 2) (elt object 1))
	  )))
    object))

(defun jsons-literal ()
  "Returns a json-literal given by the current token. A literal is either true|false|null."
  ("json-literal" (elt jsons-curr-token jsons-tokens))
  )

(defun jsons-member ()
  "Called when a member in a JSON object needs to be parsed."
  (let (member ["json-member" "key" "value"])
    (aset 1 (elt jsons-curr-token jsons-tokens))
    ;; TODO: Error checking... Should encounter a : here.
    (jsons-consume-token)
    (aset 2 (jsons-value))
    member))

(defun jsons-number ()
  "Returns a json-number given by the current token. A json-number is defined as per the num_regex
in the jsons-get-json-tokens function."
  ("json-number" (elt jsons-curr-token jsons-tokens))
  )

(defun jsons-object ()
  "Function called when a { is encountered."
  (let (token (jsons-consume-token)
	(object ["json-object" (make-hash-table)]))
    (while (not (string= token "}"))
      (if (string= token ",")
	  (setq token (jsons-consume-token))
	(let (json-mem (jsons-member))
	  (puthash (elt json-mem 1) (elt json-mem 2) (elt object 1))
	  )))
    object))

(defun jsons-string ()
  "Returns a json-string given by the current token."
  ("json-string" (elt jsons-curr-token jsons-tokens))
  )

(defun jsons-value ()
  "A value, which is either an object, array, string, number, or literal."
  (let (value ("json-value" nil)
	      (token (jsons-consume-token)))
    (cond
     ((string= token "{") (aset 1 (jsons-object)))
     ((string= token "[") (aset 1 (jsons-array)))
     ((string= token "\"") (aset 1 (jsons-string)))
     ((not (= (string-to-number token) 0)) (aset 1 (jsons-number)))
     (t (aset 1 (jsons-literal)))
     ) 
    value))

(defun parse ()
  ""
  (interactive)
  (let (var1)
    (setq var1 some)
    
    ))

(jsons-get-str-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/test_strings.in")
(jsons-get-str-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/aws.json")
(jsons-get-num-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/test_num.in")
(get-num-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/test_num.in")
(jsons-get-delim-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/rfc_object.json")

(setq jsons-tokens (jsons-get-json-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/rfc_object.json"))
