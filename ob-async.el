(require 'async)
(require 'org-id)

(defun link/org-babel-execute-src-block-async (&optional init-file)
  "Asynchronously execute the current source code block."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Init file: ")
                       user-init-file)))
  (let ((uuid (org-id-uuid))
        (name (org-element-property :name (org-element-context)))
        tempfile)
    (setq tempfile (concat "tmp-" uuid ".org"))
    (write-region (point-min) (point-max) tempfile)
    (save-excursion
      (beginning-of-line)
      (re-search-forward "#\\+END_SRC")
      (org-babel-remove-result)
      (insert (format
               "\n\n#+RESULTS:%s\n: %s"
               (if name (concat " " name) "") uuid))
      (let ((end (point)))
        (previous-line)
        (beginning-of-line)
        (org-indent-region (point) end)))
    (async-start 
     `(lambda ()
        (load-file ,init-file)
        (defun ask-user-about-lock (file opponent))
        (find-file ,tempfile)
        (goto-char ,(point))
        (org-babel-execute-src-block)
        (let ((location (org-babel-where-is-src-block-result))
              out err)
          (setq out (if (not location) ""
                      (save-excursion
                        (goto-char location)
                        (when (looking-at (concat org-babel-result-regexp ".*$"))
                          (buffer-substring-no-properties
                           (1+ (match-end 0))
                           (progn (forward-line 1) (org-babel-result-end)))))))
          (setq err (when (get-buffer "*Org-Babel Error Output*")
                      (with-current-buffer "*Org-Babel Error Output*"
                        (buffer-string))))
          (kill-buffer)
          (delete-file ,tempfile)
          (list :out out :err err)))
     `(lambda (result)
        (let ((out (plist-get result :out))
              (err (plist-get result :err)))
          (with-current-buffer ,(buffer-name)
            (save-excursion
              (org-save-outline-visibility t
                (outline-show-all)
                (goto-char (point-min))
                (when (re-search-forward ,uuid nil t)
                  (kill-whole-line)
                  (insert out)))))
          (when err
            (display-buffer
             (with-current-buffer (get-buffer-create "*Org-Babel Async Output*")
               (read-only-mode -1)
               (delete-region (point-min) (point-max))
               (insert err)
               (compilation-mode 1)
               (current-buffer)))))))))

(defun link/org-babel-execute-buffer-async (&optional init-file)
  "Asynchornous execute source code blocks in a buffer."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Init file: ")
                       user-init-file)))
  (org-babel-eval-wipe-error-buffer)
  (org-save-outline-visibility t
    (org-babel-map-executables nil
      (if (looking-at org-babel-lob-one-liner-regexp)
          (org-babel-lob-execute-maybe)
        (link/org-babel-execute-src-block-async init-file)))))

(provide 'ob-async)
