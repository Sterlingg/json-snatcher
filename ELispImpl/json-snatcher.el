;; json-snatcher.el
;; Author: Sterling Graham

;; Working on:
;;            - Finish up the parsing functions. 
;;            - Testing

(setq jsons-curr-token 0)
(setq jsons-tokens ())
(setq jsons-num-regex "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)")


(setq string_regex )

"dogs";;"[^\"\\\\[:space:]]"
(re-search-forward "[^\n\t\s]")
(re-search-forward string_regex)



"View from 15th Floor"
(defun jsons-get-json-tokens (file)
  "Retrieves all of the strings in the current buffer."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let* ((return_list ()) 
	   (delim_regex "\\([\][\\{\\}:,]\\)")
	   ;; TODO: Improve this regex. Although now it SEEMS to be working, and can be
	   ;; used to validate escapes if needed later. The second half of it is pretty
	   ;; pointless at the moment. I did it this way, so that the code closely mirrors
	   ;; the RFC.
	   (string_regex "\\(\"\\(\\([^\"\\\\\r\s\t\n]\\)*\\([\r\s\t\n]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^\r\s\t\n]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)")
	   (num_regex "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)")
	   (full_regex (concat "\\(" delim_regex "\\|" string_regex "\\|" num_regex "\\)")))
      (while (re-search-forward full_regex (point-max) "Not nil")
	(goto-char (match-end 0))
	(push (buffer-substring (match-beginning 0) (match-end 0)) return_list))
      (reverse return_list))))

(defun jsons-consume-token ()
  "Advances the parser to the next token."
  (if jsons-tokens
      (prog1 
	  (pop jsons-tokens)
	(setq jsons-curr-token (+ jsons-curr-token 1))
	) 
    nil))

(defun jsons-array ()
  "Function called when a [ is encountered."
  (let ((token (jsons-consume-token))
	(array (list "json-array" ())))
    (while (not (string= token "]"))
      (if (string= token ",")
	  (setq token (jsons-consume-token))
	(let ((json-value (jsons-value)))
	  (push json-value (elt array 1))
	  (setq token (jsons-consume-token))
	  )))
    array))

(defun jsons-literal (token)
  "Returns a json-literal given by the current token. A literal is either true|false|null."
 (list "json-literal" token)
  )

(defun jsons-member (token)
  "Called when a member in a JSON object needs to be parsed."
  (let ((member (list "json-member")))
    (setq member (cons member token))
    ;; TODO: Error checking... Should encounter a : here.
    (if  (not (string= (jsons-consume-token) ":"))
	(error "Encountered token other than : in jsons-member.")
      nil
	)
    (setq member (cons member (cons (cdr member) (jsons-value))))
    member))

(defun jsons-number (token)
  "Returns a json-number given by the current token. A json-number is defined as per the num_regex
in the jsons-get-json-tokens function."
  (list "json-number" token)
  )

(defun jsons-object ()
  "Function called when a { is encountered."
  (let*(
	(token (jsons-consume-token))
	(members (make-hash-table :test 'equal))
	(object (list "json-object" members)))
    (progn
      (while (not (string= token "}"))
	(if (not (string= token ","))
	    (let ((json-mem (jsons-member token)))
	      (puthash (elt json-mem 1) (elt json-mem 2) (elt object 1))
	      (setq token (jsons-consume-token))
	      )
	  (setq token (jsons-consume-token))))

      object
      )))

(defun jsons-string ()
  "Returns a json-string given by the current token."
  (list "json-string" (elt jsons-curr-token jsons-tokens))
  )
;;list "json-value" nil)
(defun jsons-value ()
  "A value, which is either an object, array, string, number, or literal."

  (if (jsons-is-number token)
      (list "json-value" (jsons-number token))
    
    (let ((val (list "json-value"))
	  (token (jsons-consume-token)))
      (cond
       ((string= token "{") (setq val (list val (jsons-object))))
       ((string= token "[") (setq val (list val (jsons-array))))
       ((string= token "\"") (setq val (list val (jsons-string token))))
       (t (setq val (list val (jsons-literal token))))
       )
      val)
    ))

 (defun string-integer-p (string)
   (if (string-match "\\`[-+]?[0-9]+\\'" string)
       t
     nil))

(defun jsons-is-number (str)
  "Tests to see whether str is a valid JSON number. Doesn't quite work properly,
   since it accepts strings such as 0s, but since I'm only using it to parse
   values and not validate them right now I'm using it as is.
TODO: Fix this function to work properly."
  (if (string-match jsons-num-regex str)
      t
    nil))

(defun jsons-parse (file)
  ""
  (setq jsons-tokens (jsons-get-json-tokens file))
  (let ((token (jsons-consume-token)))
    (cond
     ((string= token "{") (jsons-object))
     ((string= token "[") (jsons-array))
     (t nil))
    )
)

(defun jsons-put-string (buffer str)
  "Append the text of the region to BUFFER."
  (interactive "BAppend to buffer: \nr")
    (save-current-buffer
      (set-buffer (get-buffer-create buffer))
      (insert (prin1-to-string str))))

(setq max-lisp-eval-depth 10000)

(jsons-put-string "I can't believe it's not buffer!" (jsons-parse "~/Programming/Projects/json-snatcher/ELispImpl/Test/rfc_array.json"))

(jsons-get-json-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/aws.json")
(setq max-mini-window-height 0.30)

(setq jsons-tokens (jsons-get-json-tokens "~/Programming/Projects/json-snatcher/ELispImpl/Test/aws.json"))
(jsons-consume-token)
(setq debug-on-error t)

