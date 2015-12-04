(require 'async)
(require 'org-id)

(defun link/org-babel-execute-async ()
  "Asynchronously execute the current source code block."
  (interactive)
  (save-buffer)
  (let ((uuid (org-id-uuid))
        (name (org-element-property :name (org-element-context))))
    (save-excursion
      (re-search-forward "#\\+END_SRC")
      (org-babel-remove-result)
      (insert (format
               "\n\n#+RESULTS:%s\n: %s"
               (if name (concat " " name) "") uuid)))
    (async-start 
     `(lambda ()
        (load-file (expand-file-name "~/.emacs.d/init.el"))
        (defun ask-user-about-lock (file opponent))
        (global-auto-revert-mode -1)
        (find-file ,(buffer-file-name))
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
          (list :out out :err err)))
     `(lambda (result)
        (let ((out (plist-get result :out))
              (err (plist-get result :err)))
          (save-window-excursion
            (save-excursion
              (save-restriction
                (with-current-buffer ,(buffer-name)
                  (goto-char (point-min))
                  (when (re-search-forward ,uuid nil t)
                    (kill-whole-line)
                    (insert out))))))
          (when err
            (display-buffer
             (with-current-buffer (get-buffer-create "*Org-Babel Async Output*")
               (read-only-mode -1)
               (delete-region (point-min) (point-max))
               (insert err)
               (compilation-mode 1)
               (current-buffer)))))))))

(provide 'ob-async)
