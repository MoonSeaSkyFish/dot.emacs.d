;;
;; my-startup-screen.el
;;
;; Create Dated: <2020/06/28 02:53:08>
;; Last Updated: <2021/05/30 12:01:47>
;; 

(require 'my-org-lib)
(require 'my-lib)

(defvar my/startup-screen-favorite-directories nil)
(defvar my/startup-screen-favorite-files nil)

(defvar my/startup-screen-buffer-name "*top*")
(defvar my/startup-screen-image-ext-regex "\\.\\(png\\|jpg\\|jpeg\\|gif\\)$")
(defvar my/startup-screen-image-dir "~/.emacs.d/images/dashboard/")

(defun my/startup-screen-insert-image(file)
  "insert image"
  (setq img (create-image file))
  (setq w (window-width))
  (setq iw (car (image-size img)))
  (setq lmargin (floor (/ (- w iw) 2)))
  (goto-char 0)
  (insert "\n")
  (insert (make-string lmargin ? ))
  (insert-image img)
  (insert "\n"))

(defun my/startup-screen-insert-image-random(dir)
  "select random image file"
  (let ((lst ()))
    (dolist (f (directory-files dir))
      (if (string-match my/startup-screen-image-ext-regex f)
          (push f lst)))
    (if (> (length lst) 0)
        (my/startup-screen-insert-image
         (concat (file-name-as-directory dir) (nth (random (length lst)) lst))))))

(define-derived-mode my/startup-screen-mode org-mode my/startup-screen-buffer-name
  "startup screen mode."
  (define-key my/startup-screen-mode-map (kbd "<tab>") 'org-next-link)
  (define-key my/startup-screen-mode-map (kbd "<backtab>") 'org-previous-link)
  (define-key my/startup-screen-mode-map (kbd "<return>") 'org-open-at-point)
  )

(defun my/startup-screen-setup ()
    (when (< (length command-line-args) 2 )
      (add-hook 'after-init-hook (lambda ()
                                   ;; Display useful lists of items
                                   (my/startup-screen-insert-startupify-lists)
                                   ))
      (add-hook 'emacs-startup-hook '(lambda ()
                                       (switch-to-buffer my/startup-screen-buffer-name)
                                       (goto-char (point-min))
                                       (redisplay)))))

(defun my/startup-screen-insert-startupify-lists ()
  "start up screen widget"
  (with-current-buffer (get-buffer-create my/startup-screen-buffer-name)
    (my/startup-screen-mode)
    (insert "* Emacs\n")
    ;;;;;;;(my/startup-screen-insert-image-random my/startup-screen-image-dir)
    (insert "\n")
    (my/startup-screen-insert-menu)
    (my/startup-screen-insert-favorite-dir)
    (my/startup-screen-insert-favorite-file)
    (my/startup-screen-insert-recent-dir)
    (my/startup-screen-insert-recent-file)
    (my/startup-screen-insert-recent-org)
    (setq buffer-read-only t)
   ))

(defun my/startup-screen-insert-menu ()
  (insert "[[fopen:~/owl/1_0.owl][wiki]]")
;  (insert "[[fopen:~/orgwiki/0000-00-00-000000.org][wiki]]")
  (insert " | ")
  (insert "[[fopen:~/.emacs.d/conf.org][Config]]")
  (insert " | ")
  (insert "[[elisp:my/find-fav-file][Favorite]]")
  (insert " | ")
  (insert "[[elisp:neotree-show][Folder]]")
  (insert "\n\n"))

(defun my/startup-screen--noprint-file (file)
  (not (string-match
        "^/home/luna/.emacs.d/elpa/\\|^/home/luna/.emacs.d/bookmarks"
        file)))

(defun my/startup-screen-insert-recent-file ()
  (insert "** Recent Files\n" )
  (if  (null recentf-list)
      (insert "\n...nothing\n")
    (let ((i 0))
      (dolist (f recentf-list)
        (if (my/startup-screen--noprint-file f)
            (progn
              (if (< i 10)
                  (insert (format "- [[fopen:%s][%s]]\n" f f)))
              (setq i (+ 1 i)))))
      )
    )
  (insert "\n"))

(defun my/startup-screen-insert-recent-org ()
  (insert "** Recent OWL Files\n")
    (dolist (tt (my/recent-owl-file (my/change-text-dir "owl") 10))
    (insert "- [[fopen:" (cl-first tt) "][" (cl-second tt) "]]\n")
    )
    (insert "\n"))
  


(defun my/startup-screen-insert-recent-dir ()
  (insert "** Recent Directories\n" )
  (if (null recentf-list)
      (insert "\n...nothing\n")
    (let ((i 0) (alreadylist nil)) 
      (dolist (f recentf-list)
        (if (my/startup-screen--noprint-file f)
            (let ((find nil) (targetdir (file-name-directory f)))
              (dolist (d alreadylist)
                (if (string= targetdir d)
                    (setq find t)))
              (if (not find)
                  (progn
                    (push targetdir alreadylist)
                    (if (< i 10)
                        (insert "- [[elisp:(neotree-find \"" targetdir "\")][" targetdir "]]\n"))
                    (setq i (+ i 1))))))))
    (insert "\n")))


(defun my/startup-screen-insert-favorite-dir ()
  (insert "** Favorite Directories\n" )
  (dolist (d my/startup-screen-favorite-directories)
    (insert "- [[elisp:(neotree-find \"" (cadr d) "\")][" (car d) "]]\n"))
  (insert "\n"))

(defun my/startup-screen-insert-favorite-file ()
  (insert "** Favorite Files\n" )
  (dolist (f my/startup-screen-favorite-files)
    (insert "- [[fopen:" (cadr f) "][" (car  f) "]]\n"))
  (insert "\n"))

(defun my/recent-owl-file (dir max))
(defun my/recent-owl-file (dir max)
  "指定ディレクトリの最近更新したorgファイルの一覧のタイトルを返す。"
  (let ((lst) (db (my/org-get-keywords-in-dir dir "*.owl")))
    (maphash
       '(lambda (filename ht)
          (push (list  filename
                       (car (car (gethash "TITLE" ht)))
                       (car (car (gethash "LAST" ht))))
                lst)) db)
    (setq lst (sort lst '(lambda (a b) (string> (cl-third a) (cl-third b)))))
  ;(message "run my recent org s?%s " (length (cl-subseq lst 0 max)))
    (if lst (cl-subseq lst 0 max))))


(provide 'my-startup-screen)

;; end

