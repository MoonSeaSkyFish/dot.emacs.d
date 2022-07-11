;;; my-keys.el --- Minor Mode for My Keybindings -*- lexical-binding: t -*-

;; conao3 emacs キーマップlookup順序について
;; https://conao3.com/blog/2020-90bf-a3bb/

(defvar my/keys-overriding-map (make-sparse-keymap))
(defvar my/keys-map (make-sparse-keymap))

(defmacro my/define-key(key cmd)
  (list 'define-key 'my/keys-overriding-map key cmd))

(defun my/dynamic-binding (key)
  ""
  `(menu-item
	 ,""
	 nil
	 :filter
	 ,(lambda (&optional _)
		 (my/key-binding key))))

(defun my/minor-mode-key-binding (key)
  ""
  (let ((active-maps nil))
    (mapc (lambda (x)
	    (when (and (symbolp (car x)) (symbol-value (car x)))
	      (add-to-list 'active-maps  (lookup-key (cdr x) (kbd key)))))
	  minor-mode-map-alist )
    (make-composed-keymap active-maps)))

(defun my/key-binding (key)
  ""
  (make-composed-keymap
    (list
      (my/minor-mode-key-binding key)
      (local-key-binding (kbd key))
      (global-key-binding (kbd key)))))

(add-to-list 'emulation-mode-map-alists
	     `((my/keys . ,my/keys-overriding-map)))


(define-minor-mode my/keys
  "My keybinding."
  :lighter " MyKeys"
  :keymap my/keys-map
  :global t
  )

(provide 'my-keys)


;;; my-keys.el ends here
