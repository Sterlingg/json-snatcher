(ert-deftest pp-test-quote ()
  "Tests the rendering of `quote' symbols in `pp-to-string'."
  (should (equal (pp-to-string '(quote quote)) "'quote"))
  (should (equal (pp-to-string '((quote a) (quote b))) "('a 'b)\n"))
  (should (equal (pp-to-string '('a 'b)) "('a 'b)\n")))


(defun read-file-lines (filepath)
  "Reads the lines of a file given in filepath into an array and returns that array."
  (with-temp-buffer
    (insert-file-contents filepath)
    (split-string
     (buffer-string) "\n" t)
    ))
