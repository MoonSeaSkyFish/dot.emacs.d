;;; wakib-keys.el --- Minor Mode for Modern Keybindings -*- lexical-binding: t -*-

;; conao3 emacs キーマップlookup順序について
;; https://conao3.com/blog/2020-90bf-a3bb/

(defun my-dynamic-binding (key)
  ""
  `(menu-item
	 ,""
	 nil
	 :filter
	 ,(lambda (&optional _)
		 (my-key-binding key))))


(defun my-minor-mode-key-binding (key)
  ""
  (let ((active-maps nil))
    (mapc (lambda (x)
	    (when (and (symbolp (car x)) (symbol-value (car x)))
	      (add-to-list 'active-maps  (lookup-key (cdr x) (kbd key)))))
	  minor-mode-map-alist )
    (make-composed-keymap active-maps)))


(defun my-key-binding (key)
  ""
  (make-composed-keymap
    (list
;;      (my-minor-mode-key-binding key)
      (local-key-binding (kbd key))
      (global-key-binding (kbd key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup for keymap

(defvar my-keys-overriding-map (make-sparse-keymap) "")
(defvar my-keys-map (make-sparse-keymap) "")

(defun my-define-keys (keymap keylist)
  ""
  (interactive)
  (mapc (lambda (pair)
          (define-key keymap (kbd (car pair)) (cdr pair)))
        keylist)
  (define-key keymap (kbd "C-e") (my-dynamic-binding "C-x"))
  (define-key keymap (kbd "C-d") (my-dynamic-binding "C-c")))

(defvar my-keylist
  `(
    ("C-o" . find-file)
    ("C-S-o" . revert-buffer)
    ("C-w" . kill-current-buffer)
    ("C-q" . quote-insert)
    ("C-<next>" . next-buffer)
    ("C-<prior>" . previous-buffer)
    ("C-c" . kill-ring-save)
    ("C-x" . kill-region)
    ("C-v" . yank)
    ("C-z" . undo)
    ("C-f" . isearch-forward)
    ("C-S-f" . isearch-backward)
    ("C-r" . query-replace)
    ("C-S-r" . query-replace-regexp)
    ("C-s" . save-buffer)
    ("C-b" . switch-to-buffer)
     ("<escape>" . keyboard-quit)
     ) ;; should quit minibuffer
  "")


;; keymap 生成
(my-define-keys my-keys-overriding-map my-keylist)

;;重要そう
(add-to-list 'emulation-mode-map-alists
	     `((my-keys . ,my-keys-overriding-map)))


(define-minor-mode my-keys
  ""
  :lighter " MyKeys"
  :keymap my-keys-map
  :global t
  )

(provide 'my-test)


;;; my-keys.el ends here
