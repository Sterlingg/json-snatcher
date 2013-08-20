;; json-snatcher.el
;; Author: Sterling Graham
;; Year: 2013
;;
;; Wishlist:
;;          - Faster region lookup. Possibly using an interval tree.
;;

(setq jsons-curr-token 0)
(setq jsons-tokens ())
(setq jsons-regions ())
(setq lexical-binding t)

(defun jsons-consume-token ()
  "Returns the next token in the stream."
  (goto-char jsons-curr-token)
  (let* ((delim_regex "\\([\][\\{\\}:,]\\)")
	 ;; TODO: Improve this regex. Although now it SEEMS to be working, and can be
	 ;; used to validate escapes if needed later. The second half of the string regex is pretty
	 ;; pointless at the moment. I did it this way, so that the code closely mirrors
	 ;; the RFC.
	 (string_regex "\\(\"\\(\\([^\"\\\\\r\s\t\n]\\)*\\([\r\s\t\n]\\)*\\|\\(\\(\\\\\\\\\\)*\\\\\\(\\([^\r\s\t\n]\\|\\(u[0-9A-Fa-f]\\{4\\}\\)\\)\\)\\)\\)+\"\\)")
	 (num_regex "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)")
	 (full_regex (concat "\\(" delim_regex "\\|" string_regex "\\|" num_regex "\\)")))

    (if (re-search-forward full_regex (point-max) "Not nil")
	(progn
	  (setq jsons-curr-token (match-end 0))
	  (buffer-substring-no-properties (match-beginning 0) (match-end 0)))
      (message "Reached EOF."))))

(defun jsons-array (path)
  "Function called when a [ is encountered. Creates a new json array object that contains the
identifier \"json-array\", a list of the elements contained in the array, and the path to the
array."
  (let*(
	(token (jsons-consume-token))
	(array "json-array")
	(elements ())
	(i 0)
	(range_start jsons-curr-token)
	(range_end (point-max)))
    (while (not (string= token "]"))
      (if (not (string= token ","))
	  (let ((json-val (jsons-value token path i)))
	    (setq i (+ i 1))
	    (push json-val elements)
	    (setq token (jsons-consume-token)))
	(setq token (jsons-consume-token))))
    (setq range_end (match-end 0))
    (list array (reverse elements) path)))

(defun jsons-literal (token path)
  "Returns a json-literal given by the current token. A literal is either true|false|null."
 (list "json-literal" token path))

(defun jsons-member (token path)
  "Called when a member in a JSON object needs to be parsed."
  (let* ((member ())
	 (value token)
	 (range_start (match-beginning 0))
	 (range_end (match-end 0))
	 )
    (setq member (list "json-member" token))
    (if (not (string= (jsons-consume-token) ":"))
	(error "Encountered token other than : in jsons-member.")
      nil)

    (let ((json-val (jsons-value (jsons-consume-token) (cons value path) nil)))
      (setq member (list member (append json-val
					(list range_start range_end))))
      (setq jsons-regions (append jsons-regions (list (list range_start range_end json-val))))
    member)))

(defun jsons-number (token path)
  "Returns a json-number given by the current token. A json-number is defined as per the num_regex
in the jsons-get-tokens function."
  (list "json-number" token path))

(defun jsons-object (path)
  "Function called when a { is encountered."
  (let*(
	(token (jsons-consume-token))
	(members (make-hash-table :test 'equal))
	(object (list "json-object" members path)))
    
    (while (not (string= token "}"))
      (if (not (string= token ","))
	  (let ((json-mem (jsons-member token path)))
	    (puthash (elt (elt json-mem 0) 1) (elt json-mem 1) (elt object 1))
	    (setq token (jsons-consume-token))
	    )
	(setq token (jsons-consume-token)))
      )
    object))

(defun jsons-string (token path)
  "Returns a json-string given by the current token."
  (list "json-string" token path))

;;TODO: Refactor the if array-index statement.
(defun jsons-value (token path array-index)
  "A value, which is either an object, array, string, number, or literal. The is-array
variable is nil if not coming from an array, or the index of the value in the array that
it is contained in."
  (if array-index
      (if (jsons-is-number token)
	  (let (
		(json-num (jsons-number token (cons array-index path))))
	    (setq jsons-regions (append jsons-regions (list (list (match-beginning 0) (match-end 0) json-num))))
	    (list "json-value" json-num (list (match-beginning 0) (match-end 0)))	      
	    )
	(cond
	 ((string= token "{") (jsons-object (cons array-index path)))
	 ((string= token "[") (jsons-array (cons array-index path)))
	 ((string= (substring token 0 1) "\"") (jsons-string token (cons array-index path)))
	 (t (jsons-literal token (cons array-index path)))
	 )
	)

    (if (jsons-is-number token)
	(list "json-value" (jsons-number token path))
      (cond
       ((string= token "{") (jsons-object path))
       ((string= token "[") (jsons-array path))
       ((string= (substring token 0 1) "\"") (jsons-string token path))
       (t (jsons-literal token path)))))

  )

 (defun string-integer-p (string)
   (if (string-match "\\`[-+]?[0-9]+\\'" string)
       t
     nil))

(defun jsons-is-number (str)
  "Tests to see whether str is a valid JSON number. Doesn't quite work properly,
   since it accepts strings such as 0s, but since I'm only using it to parse
   values and not validate them right now I'm using it as is.
TODO: Fix this function to work properly."
  (progn 
    (match-end 0)
    (save-match-data
      (if (string-match "\\(-?\\(0\\|\\([1-9][[:digit:]]*\\)\\)\\(\\.[[:digit:]]+\\)?\\([eE][-+]?[[:digit:]]+\\)?\\)" str)
	  (progn
	    (match-end 0)
	    t
	    )
	nil))))

(defun jsons-parse ()
  "Parses the file given in file, returns a list of nodes representing the file."
  ;;  (setq jsons-tokens (jsons-get-tokens file))
  (with-current-buffer "aws.json"
    (progn
      (setq jsons-curr-token 0)
      (let ((token (jsons-consume-token)))
	(cond
	 ((string= token "{") (jsons-object ()))
	 ((string= token "[") (jsons-array ()))
	 (t nil))))))

(defun jsons-put-string (buffer str)
  "Append the text of the region to BUFFER."
    (save-current-buffer
      (set-buffer (get-buffer-create buffer))
      (insert (prin1-to-string str t))))

(defun jsons-print-to-buffer (buffer node)
  "Prints the given node to the buffer specified in buffer argument.
TODO: Remove extra comma printed after lists of object members, and lists of array members."
  (let ((id (elt node 0)))
    (cond
     ((string= id "json-array")
      (progn
	(jsons-put-string buffer "[")
	(mapc (lambda (x) (progn
			    (jsons-print-to-buffer buffer x)
			    (jsons-put-string buffer ",") )) (elt node 1))
	(jsons-put-string buffer "]")
      ))
     ((string= id "json-literal")
      (jsons-put-string buffer (elt node 1))
      )
     ((string= id "json-member")
      (jsons-put-string buffer (elt node 1))
      (jsons-put-string buffer ": ")
      (jsons-print-to-buffer buffer (elt node 2)))
     ((string= id "json-number")
      (jsons-put-string buffer (elt node 1)))
     ((string= id "json-object")
      (progn 
	(jsons-put-string buffer "{")
	(maphash (lambda (key value) 
		   (progn
		     (jsons-put-string buffer key)
		     (jsons-put-string buffer ":")
		     (jsons-print-to-buffer buffer value)
		     (jsons-put-string buffer ","))) (elt node 1))
      (jsons-put-string buffer "}")))
     ((string= id "json-string")
      (jsons-put-string buffer (elt node 1)))
     ((string= id "json-value")
      (jsons-print-to-buffer buffer (elt node 1)))
     (t nil))))

(defun test_regions ()
  "Function I'm using to check whether I can grab the json path from the cursor position in the json file."
  (interactive)
  (let ((i 0)
	(node nil))
    (while (< i (length jsons-regions))
      (let*	  
	  ((json_region (elt jsons-regions i))
	   (min_token (elt json_region 0))
	   (max_token (elt json_region 1)))
	(when (and (> (point) min_token) (< (point) max_token))
	  (setq node (elt json_region 2))))
      (setq i (+ i 1)))
    (message (elt node 2))))

(jsons-parse)
(jsons-put-string "Test_buffer" jsons-regions)
