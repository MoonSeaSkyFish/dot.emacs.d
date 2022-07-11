;;
;;

(setq warning-suppress-log-types '((package reinitialization)))

(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(left . 1940) default-frame-alist)
;;;
(add-to-list 'default-frame-alist '(font . "Ricty-12"))

(custom-set-faces
 '(default ((t (:background "#000522" :foreground "#dddddd")))))

;;(custom-set-faces
;; '(default ((t (:background "#fdf1d6" :foreground "#333333")))))


;(setq-default bidi-display-reordering nil)
(provide 'early-init)
;; eof
