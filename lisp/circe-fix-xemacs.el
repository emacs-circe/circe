;; XEmacs compatibility for CIRCE
;; Writte by Brian Palmer

(unless (fboundp 'compare-strings)
  (defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
    (let ((translate (if ignore-case #'upcase #'identity)))
      (equal (funcall translate (subseq str1 (or start1 0) end1))
             (funcall translate (subseq str2 (or start2 0) end2))))))

;; This adds user-defined hashtables to XEmacs
(defvar my-defined-tests nil)
(defun define-hash-table-test (name test hash)
  (add-to-list 'my-defined-tests (list name test hash)))
(defstruct my-user-defined-hashtable test equal-func hash-func values)

(defadvice make-hash-table (around my-ugly-hack activate)
  (let ((args (ad-get-args 0))
	test)
    (if (setq test (assoc (plist-get args ':test)
                          my-defined-tests))
	(setq ad-return-value
              (make-my-user-defined-hashtable :test (first test)
                                              :equal-func (second test)
                                              :hash-func (third test)))
      ad-do-it)))

(defadvice puthash (around my-ugly-puthash-hack activate)
  (if (my-user-defined-hashtable-p hash-table)
      (let ((values (my-user-defined-hashtable-values hash-table)))
	(loop for keypair in values
	      as (existing-key . existing-value) = keypair
	      if (funcall (my-user-defined-hashtable-equal-func hash-table)
                          key
                          existing-key)
	      do (setcdr keypair value) (return)
	      finally (setf (my-user-defined-hashtable-values hash-table)
                            (cons (cons key value)
                                  values))))
    ad-do-it))
(defadvice gethash (around my-ugly-gethash-hack activate)
  (if (my-user-defined-hashtable-p hash-table)
      (let ((values (my-user-defined-hashtable-values hash-table)))
	(loop for keypair in values
	      as (existing-key . existing-value) = keypair
	      if (funcall (my-user-defined-hashtable-equal-func hash-table)
                          key
                          existing-key)
	      do (return (setq ad-return-value existing-value))
	      finally (setq ad-return-value nil)))
    ad-do-it))
(defadvice remhash (around my-ugly-remhash-hack activate)
  (if (my-user-defined-hashtable-p hash-table)
      (let ((values (my-user-defined-hashtable-values hash-table)))
	(loop for keypair in values
	  as (existing-key . existing-value) = keypair
	  if (funcall (my-user-defined-hashtable-equal-func hash-table)
                      key
                      existing-key)
	  do (setq ad-return-value
                   (setf (my-user-defined-hashtable-values hash-table)
                         (remassoc existing-key values)))))
    ad-do-it))
(defadvice maphash (around my-ugly-maphash-hack activate)
  (if (my-user-defined-hashtable-p hash-table)
      (loop for (key . value) in (my-user-defined-hashtable-values hash-table)
            do (funcall function key value))
    ad-do-it))

(defadvice hash-table-count (around my-ugly-hash-table-count-hack activate)
  (if (my-user-defined-hashtable-p hash-table)
      (setq ad-return-value (length
                             (my-user-defined-hashtable-values hash-table)))
    ad-do-it))

(provide 'circe-fix-xemacs)
