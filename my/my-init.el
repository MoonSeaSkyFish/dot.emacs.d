;; -*- coding: utf-8; lexical-binding: t -*-

;;(defmacro profile-proc(&rest lst) `(progn ,@lst))
(defmacro profile-proc(&optional &rest _lst))
(profile-proc
  (require 'profiler)
  (profiler-start 'cpu))

(defconst my-saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)  
(setq gc-cons-threshold most-positive-fixnum)

(eval-and-compile
  (custom-set-variables '(warning-suppress-types '((comp)))))

(eval-and-compile
  (require 'tea)
  (defalias '!cset 'custom-set-variables)
  (defalias '!ewhen 'eval-when-compile)
  (defalias '!eand 'eval-and-compile)
  (defalias '!after 'with-eval-after-load)
  (defalias '!exec 'call-process-shell-command)
  (defmacro !emacs.d (path)
    (list 'expand-file-name
      (list 'locate-user-emacs-file path))))

(add-to-list 'load-path (!ewhen (!emacs.d "personal")))
(add-to-list 'load-path (!ewhen (!emacs.d "lisp")))
(push (!ewhen (expand-file-name "~/info/")) Info-default-directory-list)
(defconst my/init-file-name (!ewhen (!emacs.d "my/my-init.el")))
(defconst my/org-init-file-name (!ewhen (!emacs.d "my/conf.org")))

(defmacro on(&rest lst) `(progn ,@lst))
(defmacro off(&optional &rest _lst))
(defmacro install-off(&optional &rest _lst))
(defmacro install-on(&rest lst) `(progn ,@lst))

(defmacro my/change-ext(fn ext)
  "拡張子を変更して返す。引数の拡張子に .は不要。"
  (list 'concat (list 'file-name-sans-extension fn) "." ext))

;; 非同期に行う設定のリスト
(defvar my-delayed-configurations nil)

;; 0.1 秒ずつ間隔を開けながら消化
(defvar my-delayed-configuration-timer nil)
(add-hook 'after-init-hook
  (lambda ()
    (setq my-delayed-configuration-timer
      (run-with-timer
        0.1 0.1 ; 0.1 秒ごとに
        (lambda ()
          (if my-delayed-configurations ; まだやることがあれば
            (eval (pop my-delayed-configurations)) ; 一個やる
            (cancel-timer my-delayed-configuration-timer)))))))
(defmacro with-delayed-execution (&rest body)
  (declare (indent 0))
  `(push ',(cons 'progn body) my-delayed-configurations))
(defalias '!delay 'with-delayed-execution)

(on
 (defun my/org-babel-init-file()
   (interactive)
   (org-babel-tangle-file my/org-init-file-name my/init-file-name)
   (byte-compile-file my/init-file-name))
 (add-hook 'after-save-hook
   #'(lambda ()
       (if (string= buffer-file-name my/org-init-file-name)
         (my/org-babel-init-file)))))

(off
  (defun find-conf()
    (interactive)
    (find-file my/org-init-file-name)))

(on (require 'cl-lib))

(on (require 'posframe))

(!delay (require 'org))

(install-off
  (!ewhen
    (defvar my/favorite-packages)
    (setq package-archives
      '(("org"   . "https://orgmode.org/elpa/")
         ("melpa" . "https://melpa.org/packages/")
         ("gnu"   . "https://elpa.gnu.org/packages/")))
    (on (package-refresh-contents)) ;;たまにはrefreshしませう
    (package-initialize)
    (setq my/favorite-packages
      '(
         ;;profiler
         ;;use-package
         ;;ivy swiper counsel         
         ;; -- vertico 関係 --
         vertico           
         consult orderless marginalia ;; kind-icon affeなど
         embark
         corfu cape ;; companyのかわり
         fussy
         ;; ------------------
         projectile ;;プロジェクト管理
         ;;elscreen
         ;;japanese-holidays
         ;;recentf-ext
         magit
         ;;eacl  ;;git-completeと同じgit管理下のプロジェクト保管エンジン
         ;;auto-complete
         migemo
         ddskk
         ;;smart-jump
         ;;dumb-jump
         web-mode
         sass-mode
         paredit
         smartparens
         ;;emmet-mode
         ;;---programing系
         ;;rust-mode
         nim-mode
         lsp-mode
         typescript-mode
         lua-mode
         ;;----
         posframe
         ;;----- 
         mozc
         ;;mozc-popup
         mozc-cand-posframe
         ;;-----
         rainbow-delimiters
         company company-box
         yasnippet
         yasnippet-snippets
         ;;;ivy-yasnippet
         which-key
         which-key-posframe
         highlight-indent-guides
         expand-region
         imenu-list
         ;;auto-dim-other-buffers
         ;;hydra
         ;;neotree
         ;;treemacs
         multiple-cursors
         ;;pangu-spacing
         ;;dashboard
         ;;indent-guide
         quickrun
         vterm
         reformatter
         ;;evil
         ;; - org-mode 関連 -
         org-bullets
         ;;org-drill org-journal
         ;;ox-hugo
         ob-nim
         ;;org-sidebar
         org-roam
         ))
    ;;インストールする
    (dolist (package my/favorite-packages)
      ;;(message "%s" package)
      (unless (package-installed-p package)
        (package-install package)))))

(on
  (defun my/package-list()
    (interactive)
    (setq package-archives
      '(("org"   . "https://orgmode.org/elpa/")
         ("melpa" . "https://melpa.org/packages/")
         ("gnu"   . "https://elpa.gnu.org/packages/")))
    (package-refresh-contents)
    (package-initialize)
    (package-list-packages)))

(!delay (require 'server)
  (unless (server-running-p) (server-start)))

(on
  (setq my/changelog-filename "~/txt/change.log"))

(on
  (defvar my/rsrc-fav-file-list
    '(("early-init.el" "~/.emacs.d/early-init.el")
       ("edit favfile" "~/.emacs.d/conf.org" "my/rsrc-fav-file-list")
       ("qmk qtea" "~/qmk_firmware/keyboards/hhkb/ansi/keymaps/qtea/keymap.c")
       ("openbox menu.xml" "~/.config/openbox/menu.xml")
       ("openbox rc.xml" "~/.config/openbox/rc.xml")
       ("openbox autostart" "~/.config/openbox/autostart"))))

(on
  (defvar my/rsrc-fav-dir-list
    '(("pg" "/home/pg/")
       (".emacs.d" "~/.emacs.d/")
       ("Templates" "~/Templates"))))

(on
  (setq garbage-collection-messages t)
  (setq custom-file
    (!ewhen (!emacs.d "custom.el"))))

(on
  (setq user-full-name "Sorao Tsukiumi")
  (setq user-mail-address "moon.sea.sky.fish@gmail.com"))

(on
  (setq initial-scratch-message  ";; --- scratch ---\n")
  ;;(setq initial-major-mode 'fundamental-mode)
  ;;(setq initial-major-mode 'org-mode)
  (defun display-startup-echo-area-message () (message "")))

(off
  (setq initial-scratch-message
    (concat
      "* Emacs\n"
      "** Wellcome!\n"
      )))

(on
  (setq inhibit-startup-message t)
  (setq-default bidi-display-reordering nil)
  (setq ring-bell-function 'ignore)
  (setq-default tab-width 2 indent-tabs-mode nil)
  (setq-default indent-tabs-mode nil)
  (setq echo-keystrokes 0.1))

(on (run-with-idle-timer 60.0 t #'garbage-collect));

(on
  (setq scroll-conservatively 32)
  (setq scroll-step 1)
  (setq scroll-margin 0))

(on
  (fringe-mode (cons 5 1))
  (setq-default indicate-buffer-boundaries '((bottom . left)))
  (setq-default mode-line-format 
    (list mode-line-mule-info mode-line-modified "  %b  %m  %l:%C")))

(on
  (add-to-list 'default-frame-alist '(cursor-type . bar))
  (!delay
    (global-hl-line-mode t)))

(on
  (setq backup-directory-alist
    (cons (cons ".*" (expand-file-name "~/big/.backup/emacs"))
      backup-directory-alist))
  (setq auto-save-list-file-prefix
    "~/big/.backup/emacs/auto-save-list")
  (setq auto-save-file-name-transforms
    `((".*", (expand-file-name "~/big/.backup/emacs") t))))

(on
  (set-language-environment "Japanese")
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8))

(on
  (add-to-list 'default-frame-alist '(cursor-color . "#c0c0c0"))
  (add-to-list 'default-frame-alist '(mouse-color . "#ff0000"))
  (custom-set-faces '(hl-line ((t (:background "#222255")))))
  (set-face-background 'region "#3030a0")
  (set-frame-parameter nil 'alpha 90)
  (set-face-foreground 'link "#A1D6E2")
  (set-face-foreground 'mode-line "#FFFFFF")
  (set-face-background 'mode-line "#000000")
  (set-face-foreground 'mode-line-inactive "#000000")
  (set-face-foreground 'font-lock-comment-face "#dd9933")
  (set-face-foreground 'font-lock-comment-delimiter-face "#dd9933")
  (set-face-foreground 'font-lock-string-face "#33AA33")
  (set-face-foreground 'font-lock-keyword-face "#00aaff")
  (set-face-foreground 'font-lock-constant-face "#b1f9d0")
  (set-face-foreground 'font-lock-doc-face "#ff82b2")
  (set-face-foreground 'font-lock-function-name-face "#aaffaa")
  (set-face-foreground 'font-lock-builtin-face "#ffdd44")
  (set-face-foreground 'font-lock-negation-char-face "#ffff00")
  (set-face-foreground 'font-lock-preprocessor-face "#ff0000")
  (set-face-foreground 'font-lock-regexp-grouping-backslash "#ff00ff")
  (set-face-foreground 'font-lock-regexp-grouping-construct "#00ffff")
  (set-face-foreground 'font-lock-type-face "#ff9999")
  (set-face-foreground 'font-lock-variable-name-face "#aaaaff")
  (set-face-foreground 'font-lock-warning-face "#ffff00")
  (set-face-foreground 'minibuffer-prompt "#c0c0c0")
  (set-face-foreground 'isearch-fail "#ff0000"))

(on (require 'my-keys)  (my/keys 1))

(on
  (defun my/other-window-find-file(fn)
    (other-window 1)(find-file fn)))

(on
  (add-hook 'find-file-hook 'whitespace-mode)
  (!after 'whitespace
    (setq-default whitespace-style '(face tabs tab-mark space-before-tab))
    (setq-default whitespace-display-mappings
      '((tab-mark   ?\t   [?\x21E5 ?\t] [?\\ ?\t])))
    (set-face-foreground 'whitespace-tab "#008080")
    (set-face-background 'whitespace-tab nil)))

(on
  (add-hook 'find-file-hook #'(lambda () (linum-mode 1))))

(on
  (set-face-attribute 'show-paren-match nil
    :background "#333333"
    :foreground nil
    :underline t
    :bold t
    :inverse-video nil))

(on
 (delete-selection-mode))



(on
  (!cset
    '(which-key-separator ":")
    '(which-key-prefix-prefix "")
    '(which-key-idle-delay 0.1)
    '(which-key-idle-secondary-delay 0.1)
    '(which-key-max-display-columns 1)
    '(which-key-posframe-poshandler 'posframe-poshandler-point-bottom-left-corner))
  (which-key-mode)
  (which-key-posframe-mode))

(on
  ;;(defalias 'my/define-key 'global-set-key)
  (defvar my/menu-key "<muhenkan>")
  (defmacro my/menu-group(key desc)
    (list 'which-key-add-key-based-replacements
      (list 'concat my/menu-key key) desc))
  (defmacro my/menu-set (map key desc cmd)
    (list 'progn
      (list 'define-key map
        (list 'kbd (list 'concat my/menu-key key)) cmd)
      (list 'my/menu-group key desc)))
  (defmacro my/g-menu-set (key desc cmd)
    (list 'progn
      (list 'my/define-key 
        (list 'kbd (list 'concat my/menu-key key)) cmd)
      (list 'my/menu-group key desc))))

(on
  (setq default-input-method "japanese-mozc")
  ;;(setq-default mozc-candidate-style 'echo-area)
  (!after 'posframe
    (require 'mozc-cand-posframe)
    (setq-default mozc-candidate-style 'posframe)
    (set-face-attribute 'mozc-cand-posframe-normal-face nil
      :foreground "#ffeeff"
      :background "#335577")
    (set-face-attribute 'mozc-cand-posframe-focused-face nil
      :foreground "#335577"
      :background "#ccffcc")
    (set-face-attribute 'mozc-cand-posframe-footer-face nil
      :foreground "#ffeeff"
      :background "#335577")))

(on  
  (!after 'mozc
    (my/menu-set mozc-mode-map "ff" "開く" #'find-file)))

(on
  (!cset '(imenu-list-position 'left)
    '(imenu-list-size 0.15)
  ))

(on (!delay (vertico-mode))
  (!cset '(completion-styles '(orderless))
    '(corfu-auto t)))

(off (require 'projectile))

(on 
  (my/define-key (kbd "C->") 'mc/mark-next-like-this)
  (my/define-key (kbd "C-<") 'mc/mark-previous-like-this))

(on
  (!after 'rainbow-delimiters
    (set-face-foreground 'rainbow-delimiters-depth-1-face "#FFAAAA")
    (set-face-foreground 'rainbow-delimiters-depth-2-face "#00DD00")
    (set-face-foreground 'rainbow-delimiters-depth-3-face "#FF3333")
    (set-face-foreground 'rainbow-delimiters-depth-4-face "#FFFF00")
    (set-face-foreground 'rainbow-delimiters-depth-5-face "#00FFFF")
    (set-face-foreground 'rainbow-delimiters-depth-6-face "#FF00FF")
    (set-face-foreground 'rainbow-delimiters-depth-7-face "#0000FF")
    (set-face-foreground 'rainbow-delimiters-depth-8-face "#99EE88")
    (set-face-foreground 'rainbow-delimiters-depth-9-face "#9999FF"))
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode) 
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode))

(on
  (defun my/highlight-indent-guides--bitmap-line (width height _crep zrep)
    (let*
      ((left (/ (- width 2) 2))
        (right (- width left 2))
        (row (append (make-list left zrep)
               (make-list 1 " 10000 25535 25535") ;; rgb 0-65535
               (make-list right zrep))) rows)
      (dotimes (_i height rows)
        (setq rows (cons row rows)))))
  (setq-default highlight-indent-guides-bitmap-function
    'my/highlight-indent-guides--bitmap-line)
  (setq-default highlight-indent-guides-method 'bitmap)
  (add-hook 'nim-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'cperl-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-indent-guides-mode))

(off
  (!delay
    (require 'auto-dim-other-buffers)
    (set-face-attribute 'auto-dim-other-buffers-face nil :background "#112")
    (auto-dim-other-buffers-mode t)))

(off
 (require 'lsp-mode))

(on
  (!after 'paredit
    (define-key paredit-mode-map (kbd "C-j") nil))
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(on
  (require 'smartparens-config)
  ;;(autoload 'nim-mode "smartparens-config" nil t)
  (!ewhen  
    (require 'cperl-mode))
  (!eand
    (add-hook 'nim-mode-hook
      #'(lambda ()
          (sp-local-pair 'nim-mode "#[" "]#")
          (sp-local-pair 'nim-mode "\"\"\"" "\"\"\"")
          (smartparens-mode)))
    (add-hook 'cperl-mode-hook
      #'(lambda ()
          (define-key cperl-mode-map "{" 'nil)
          (smartparens-mode)))))

(on
  (!delay
    (reformatter-define nim-format
      :program "~/.emacs.d/personal/bin/nimpretty-stdinout"
      :lighter " DF")
    (reformatter-define ts-format
      :program "prettier"
      :args '("--parser=typescript")
      :lighter " DF")
    (reformatter-define html-format
      :program "prettier"
      :args '("--parser=html")
      :lighter " DF")))

(on
  (autoload 'migemo-init "migemo")
  (!after
    (!cset
      '(migemo-command "/usr/bin/cmigemo") ; HERE cmigemoバイナリ
      '(migemo-options '("-q" "--emacs"))
      '(migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict") ; HERE Migemo辞書
      '(migemo-user-dictionary nil)
      '(migemo-regex-dictionary nil)
      '(migemo-coding-system 'utf-8-unix)))
  (!delay (migemo-init)))

(on
  (autoload 'yas-expand "yasnippet" nil t)
  (autoload 'yas-minor-mode "yasnippet" nil t)
  (setq yas-snippet-dirs
    `(,(!ewhen (!emacs.d "mysnippets"))
       ,(!ewhen (!emacs.d "snippets"))))
  (add-hook 'org-mode-hook  #'(lambda()(run-at-time "1 sec" nil #'yas-minor-mode)))
  (add-hook 'nim-mode-hook #'yas-minor-mode))

(on (!ewhen (require 'company)))
(on
  (!after 'company
    (!cset
      '(company-idle-delay  0)
      '(company-minimum-prefix-length  1)
      '(company-selection-wrap-around  t)
      '(company-transformers  '(company-sort-by-occurrence))
      '(company-dabbrev-downcase  nil))
    (add-hook 'company-mode-hook #'company-box-mode)
    (set-face-attribute 'company-tooltip nil
      :foreground "black" :background "lightgrey")
    (set-face-attribute 'company-tooltip-common nil
      :foreground "black" :background "lightgrey")
    (set-face-attribute 'company-tooltip-common-selection nil
      :foreground "white" :background "steelblue")
    (set-face-attribute 'company-tooltip-selection nil
      :foreground "black" :background "steelblue")
    (set-face-attribute 'company-preview-common nil
      :background nil :foreground "lightgrey" :underline t)
    (set-face-attribute 'company-scrollbar-fg nil
      :background "orange")
    (set-face-attribute 'company-scrollbar-bg nil
      :background "gray40")
    (define-key company-active-map [tab] 'yas-expand)))

(off (require 'expand-region))

(on
  (autoload 'my/time-stamp "my-timestamp" nil t)
  (add-hook 'before-save-hook 'my/time-stamp)
  (require 'my-vertico-extend))

(on
  (defun my/quickrun ()
    (interactive)
    (let ((cmd nil) (fn buffer-file-name))
      (cond
        ((eq major-mode 'cperl-mode) (setq cmd "perl"))
        ((eq major-mode 'perl-mode) (setq cmd "perl"))
        ((eq major-mode 'nim-mode) (setq cmd "nim c -r"))
        ((eq major-mode 'qmk-mode) (setq cmd "qmk compile -kb hhkb/ansi -km qtea") (setq fn ""))
        ((eq major-mode 'python-mode) (setq cmd "python")))
      (if cmd
        (my/vterm-cd-cmd default-directory (concat cmd " " fn))))))

(on
  (defun my/consult-line(arg)
    (interactive "p")
    (cond
      ((= arg 4) (swiper-thing-at-point))
      (t (consult-line)))))

(on
  (setq lisp-indent-offset 2))

(on
  (autoload 'cperl-mode "cperl-mode" nil t)
  (defalias 'perl-mode 'cperl-mode)
  (add-hook 'cperl-mode-hook #'company-mode)
  (!after 'cperl-mode
  (my/menu-set cperl-mode-map "cc" "Perl Run" #'my/quickrun)))

(on
  (!ewhen (require 'python))
  (autoload 'python-mode "python" nil t)
  (!after 'python
    (my/menu-set python-mode-map "cc" "Python Run" #'my/quickrun)
    (setq-default python-indent-offset 2)))





(on
  (!ewhen (require 'vterm))
  (autoload 'vterm-send-string  "vterm" nil t)
  (autoload 'my/vterm-sendline  "vterm" nil t)
  (autoload 'my/vterm-cd-cmd  "vterm" nil t)
  (autoload 'my/vterm-sendcmd  "vterm" nil t)
  (defalias 'vtx 'vterm-other-window)
  (!after 'vterm
    (setq-default vterm-max-scrollback  10000)
    (set-face-foreground 'vterm-color-black   "#2e3436")  ;; 0 - ?
    (set-face-foreground 'vterm-color-red     "#aabbff")  ;; 1
    (set-face-foreground 'vterm-color-green   "#4e9a06")  ;; 2 - exe
    (set-face-foreground 'vterm-color-yellow  "#c4a000")  ;; 3
    (set-face-foreground 'vterm-color-blue    "#3465A4")  ;; 4 - directory
    (set-face-foreground 'vterm-color-magenta "#75507B")  ;; 5
    (set-face-foreground 'vterm-color-cyan    "#ce5c00")  ;; 6
    (set-face-foreground 'vterm-color-white   "#babdb9")  ;; 7
    (set-face-background 'vterm-color-black   "#555753")  ;; 8
    (set-face-background 'vterm-color-red     "#ef2929")  ;;9
    (set-face-background 'vterm-color-green   "#8ae234")  ;;10
    (set-face-background 'vterm-color-yellow  "#fce94f")  ;;11
    (set-face-background 'vterm-color-blue    "#729fcf")  ;;12
    (set-face-background 'vterm-color-magenta "#ad7fa8")  ;;13
    (set-face-background 'vterm-color-cyan    "#fcaf3e")  ;;14
    (set-face-background 'vterm-color-white   "#eeeeec")  ;;15
    (define-key vterm-mode-map (kbd "C-b") 'switch-to-buffer)
    (define-key vterm-mode-map (kbd "C-w") 'other-window)
    (push (list "my/other-window-find-file" 'my/other-window-find-file) vterm-eval-cmds)
    (defun my/vterm-sendline (str)
      (vterm-send-string (concat str "\n")))
    (defun my/vterm-sendstr (str)
      (vterm-other-window)
      (vterm-send-string str))
    (defun my/vterm-sendcmd (str)
      (switch-to-buffer-other-window "*vterm*")
      (my/vterm-sendline str) )
    (defun my/vterm-cd-bufferdir ()
      (interactive)
      (my/vterm-sendcmd (concat "cd " default-directory)))
    (defun my/vterm-cd-cmd (dir cmd)
      (vterm-other-window)
      (my/vterm-sendline (concat "cd " dir))
      (my/vterm-sendline cmd))
    (defun my/vterm-sendstr-interactive (arg) (interactive "svterm: ")
      (my/vterm-sendstr arg))
    (define-key vterm-mode-map (kbd "C-c C-s")
      #'(lambda (s) (interactive "svterm: ")(vterm-send-string s)))
    ))

(on
  (!ewhen (require 'nim-mode))
  (!after 'nim-mode
    (add-hook 'nim-mode-hook #'lsp)
    (add-hook 'nim-mode-hook #'company-mode)
    (add-hook 'nim-mode-hook #'nim-format-on-save-mode)))

(on
  (!after 'nim-mode
    (!eand
      (defun my/nim-newline-and-indent ()
        "空行の次の行はインデントしない"
        (interactive)
        (let
          ((no-indent
             (string= ""
               (string-trim
                 (buffer-substring-no-properties
                   (point-at-bol) (point-at-eol))))))
          (if no-indent (newline)
            (newline-and-indent))))
      (define-key nim-mode-map (kbd "RET") 'my/nim-newline-and-indent)
      (define-key nim-mode-map (kbd "<C-return>")
        #'(lambda () (interactive) (end-of-line) (my/nim-newline-and-indent)))
      (define-key nim-mode-map (kbd "<M-return>")
        #'(lambda () (interactive)
            (forward-line -1) (end-of-line) (my/nim-newline-and-indent))))))

(on
  (!after 'nim-mode
    (defun my/find-nimblefile (pdir)
      "上ディレクトリに向かってxxx.nimble探す"
      (let
        ((dir (file-name-as-directory pdir))
          (pre-path) (pos-path)
          (loop t) (find nil))
        (while loop
          (if (string-match "^\\(.*/\\)\\([^/]+\\)/$" dir)
            (progn
              (setq pre-path (match-string 1 dir))
              (setq pos-path (match-string 2 dir))
              (if (file-exists-p (concat dir pos-path ".nimble"))
                (progn
                  (setq find t)
                  (setq loop nil))            
                (if (or (string= pre-path "/") (string= pre-path ""))
                  (setq loop nil)
                  (setq dir pre-path))))
            (setq loop nil)))
        (if find dir nil)))
    (!eand
      (defun my/nim-build ()
        (interactive)
        (let ((dir (my/find-nimblefile default-directory)))
          (if dir
            (my/vterm-cd-cmd dir "nimble build")
            (message "not found nimblefile."))))
      (define-key nim-mode-map (kbd "<f5>") 'my/nim-compile)
      (define-key nim-mode-map (kbd "<f6>") 'my/nim-build)
      (my/menu-set nim-mode-map "cc" "nim c -r " #'my/quickrun)
      (my/menu-set nim-mode-map "cb" "nimble build" 'my/nim-build))))

(on
  (!after 'web-mode
    (setq-default web-mode-markup-indent-offset 2)
    (setq-default web-mode-code-indent-offset 2)
    (setq-default web-mode-css-indent-offset 2)
    (setq-default web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
         ("blade"  . "\\.blade\\."))))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(on
 (!after 'typescript-mode
   (setq-default typescript-indent-level 2))

  ;;(my/vterm-cd-cmd default-directory buffer-file-name)
  ;;(setq-default typescript-mode-hook lsp-deferred)
  ;;(setq-default typescript-mode-hook ts-format-on-save-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

(off
 (require 'sass-mode))





(on
  (define-derived-mode qmk-mode c-mode "qmk"
    "Major mode for edit qmk."
    (my/menu-set qmk-mode-map "cc" "qmk" #'my/quickrun)))

(on
  (defvar my/orgdir "/home/text/org/")
  (defvar my/org-diary-dir (!ewhen (expand-file-name "diary" my/orgdir)))
  (setq-default org-startup-truncated nil)
  (setq-default org-startup-indented t)
  (setq-default org-level-color-stars-only nil)
  (setq-default org-startup-folded nil)
  (setq-default org-hide-leading-stars t))

(on
  (!after 'org
    (defun my/diary-grumble-template ()
      "ぼやきテンプレ - 時刻のみのテンプレがないため"
      (format-time-string "- [%02H:%02M] %?"))
    (defun my/diary-org-file ()
      "日記ファイル名を返す、なきゃつくる。shellでリダイレクトしてがんばる"
      (let ((f (expand-file-name  (format-time-string "%Y-%02m-%02d.org") my/org-diary-dir )))
        (if (not (file-exists-p f))
    (shell-command-to-string 
       (concat "echo '#+TITLE: " (format-time-string "%Y年%02m月%02d日 （%a）'")" > " f
         ";echo '* Diary' >>" f ";echo '* Grumble' >>" f))) f))))

(on
  (defvar my/orgfont "Ricty-12")
  (!after 'org
    (set-face-attribute 'org-level-1 nil :bold nil :foreground "#b58900")
    (set-face-attribute 'org-level-2 nil :bold nil :foreground "#dc322f")
    (set-face-attribute 'org-level-3 nil :bold nil :foreground "#268bd2")
    (set-face-attribute 'org-level-4 nil :bold nil :foreground "#d33682")
    (set-face-attribute 'org-level-5 nil :bold nil :foreground "#6c71c4")
    (set-face-attribute 'org-level-6 nil :bold nil :foreground "#cb4b16")
    (set-face-attribute 'org-level-7 nil :bold nil :foreground "#2aa198")
    (set-face-attribute 'org-level-8 nil :bold nil :foreground "#859900")
    (set-face-attribute 'org-block-begin-line nil :family my/orgfont :bold nil :foreground "#909090")
    (set-face-attribute 'org-block nil  :family my/orgfont :bold nil :foreground "#aaffee")
    (set-face-attribute 'org-block-end-line  nil :family my/orgfont :bold nil :foreground "#909090")
    (set-face-attribute 'org-meta-line  nil :family my/orgfont :bold nil :foreground "#90aa90")
    (set-face-attribute 'org-document-info  nil :bold nil :foreground "#90aa90")
    (set-face-attribute 'org-document-info-keyword nil :family my/orgfont
      :bold nil :foreground "#90aa90")
    (set-face-attribute 'org-document-title nil
      :bold t :foreground "orange" :height 150)
    (set-face-attribute 'org-table  nil :family my/orgfont :bold nil :foreground "#ffccaa")))

(on
  (add-hook 'org-capture-mode-hook #'(lambda () (setq-local header-line-format nil)))
  (setq-default org-capture-templates
    '(
    ;;   ("m" "モーニング・ページ" )
       ("d" "おれの日記" entry (file+headline my/diary-org-file "Diary") "** %?")
       ("g" "ぼやき" item (file+headline my/diary-org-file "Grumble")
         (function my/diary-grumble-template)))))



(on
  (!after 'org
    (setq-default org-bullets-bullet-list '("✔"))
    (add-hook 'org-mode-hook #'(lambda () (org-bullets-mode 1)))
 ;(require 'ob-nim)
 ))

(tea org-roam)
(on
  (!after 'org-roam
    (!cset
      '(org-roam-directory  (!ewhen (expand-file-name "roam" my/orgdir)))
      '(org-roam-db-location (!ewhen (expand-file-name "roam.db" org-roam-directory)))
      '(org-roam-index-file (!ewhen (expand-file-name "0000-index.org" org-roam-directory)))
      '(org-roam-capture-templates
         `(("d" "default" plain "%?"
             :target
             (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
               ,(concat
                 "#+title: ${title}\n"
                 "#+date: \n"
                 "#+" "last: 0000-00-00\n"
                 "#+labels: \n\n"
                 "* "))
             :unnarrowed t)))
      nil)))

(on
  (my/define-key [henkan] #'toggle-input-method)
  (define-key minibuffer-local-map [henkan] 'toggle-input-method)
  ;;(my/define-key (kbd "C-a") #'switch-to-buffer) ;;home
  (my/define-key (kbd "C-b") #'switch-to-buffer)
  (my/define-key (kbd "C-c") #'kill-ring-save) ;; ----- C-c copy !! -----
  ;;(my/define-key (kbd "C-d") #'switch-to-buffer) ;;del
  ;;(my/define-key (kbd "C-e") #'kill-ring-save) ;;end
  (my/define-key (kbd "C-f") #'consult-line)
  ;;(my/define-key (kbd "C-g") #'ab) ;;abort?
  ;;(my/define-key (kbd "C-h") #'ignore)
  ;;(my/define-key (kbd "C-i") #'ab) ;;tab
  ;;(my/define-key (kbd "C-j") #'ignore)
  (my/define-key (kbd "C-k") #'ignore)
  (my/define-key (kbd "C-l") #'ignore)
  ;;(my/define-key (kbd "C-m") #'ignore)
  (my/define-key (kbd "C-n") #'ignore)
  (my/define-key (kbd "C-o") #'find-file)
  (my/define-key (kbd "C-p") #'ignore)
  (my/define-key (kbd "C-q") (my/dynamic-binding "C-c")) ;; C-c remap
  (my/define-key (kbd "C-r") #'query-replace)
  (my/define-key (kbd "C-s") #'save-buffer)
  (my/define-key (kbd "C-t") #'ignore)
  ;;(my/define-key (kbd "C-u") #'ignore)
  (my/define-key (kbd "C-v") #'yank)
  (my/define-key (kbd "C-w") #'other-window)
  (my/define-key (kbd "C-x") #'kill-region) ;; ----- C-x cut !! -----
  (my/define-key (kbd "C-y") #'ignore)
  (my/define-key (kbd "C-z") #'undo)
  (my/define-key (kbd "C-=") #'er/expand-region)
  ;; 記号
  (my/define-key (kbd "C-/") #'ignore)

  ;; (my/define-key (kbd "C-") #'ignore)
  ;; (my/define-key (kbd "C-") #'ignore)
  ;; (my/define-key (kbd "C-") #'ignore)
  ;; (my/define-key (kbd "C-") #'ignore)
  ;; (my/define-key (kbd "C-") #'ignore)
  ;; (my/define-key (kbd "C-") #'ignore)
  ;; (my/define-key (kbd "C-") #'ignore)

  (my/define-key (kbd "C-S-f") #'isearch-backward)
  (my/define-key "\M-w" #'ignore)

  ;; p q t n y ;; たくさんprefix-keyつくっても意味はないんじゃないか
  ;;(define-prefix-command 'ctl-p-map)
  ;;(define-prefix-command 'ctl-t-map)
  ;;(define-prefix-command 'ctl-n-map)
  ;;(define-prefix-command 'ctl-y-map)
  ;(-key "\C-p" #'ctl-p-map) ;;
  ;;(my/define-key "\C-t" #'ctl-t-map) ;; 削除系
  ;;(my/define-key "\C-k" #'ctl-n-map) ;; カット系
  ;;(my/defin-key "\C-y" #'ctl-y-map) ;; コピー系
  ;;(my/defin-key "\C-n" #'ctl-y-map)ジャンプ系


  ) ;; end on

(on (defun my/sample() (interactive)(message "test")))

(on
  ;;(define-key global-map (kbd "<muhenkan><muhenkan>")  #'my/sample)
  (my/menu-group "z" "システム")
  (my/g-menu-set "zt" "起動時間" #'emacs-init-time)
  (my/g-menu-set "zc" "設定"
    #'(lambda () (interactive) (find-file my/org-init-file-name)))
  (my/menu-group "zy" "Prefix")
  (my/g-menu-set "zyx" "C-x" (my/dynamic-binding "C-x"))
  (my/g-menu-set "zyh" "C-h" help-map)
  (my/g-menu-set "zyc" "C-c" (my/dynamic-binding "C-c"))
  (my/g-menu-set "zye" "esc" esc-map)
  (my/g-menu-set "zz" "Emacs終了" #'save-buffers-kill-terminal))

(on
  (my/menu-group "f" "ファイル")
  (my/g-menu-set "fa" "全て保存" #'save-some-buffers)
  (my/g-menu-set "fb" "sample" #'my/sample)
  (my/g-menu-set "fc" "conf.org"
    #'(lambda () (interactive) (find-file my/org-init-file-name)))
  (my/g-menu-set "ff" "開く" #'find-file)
  (my/g-menu-set "fi" "ファイル挿入" #'insert-file)
  (my/g-menu-set "fk" "閉じる" #'kill-buffer)
  (my/g-menu-set "fr" "履歴" #'counsel-recentf)
  (my/g-menu-set "fs" "保存" #'save-buffer)
  (my/g-menu-set "fw" "別名で保存" #'write-file)
  (my/g-menu-set "fx" "文字・改行コード変更" #'set-buffer-file-coding-system)
  (my/g-menu-set "fy" "コード指定して開き直す" #'revert-buffer-with-coding-system)
  (my/g-menu-set "fb" "FavFile" #'my/find-fav-file)
  (my/g-menu-set "fd" "FavDir" #'my/find-fav-dir)
  (my/g-menu-set "fz" "ダミー" #'ignore))

(on
  (autoload 'my/copy-this-line "my-editcommand" nil t)
  (autoload 'my/copy-this-word "my-editcommand" nil t)
  (my/menu-group "e" "編集")
  (my/g-menu-set "ec" "行コピー" #'my/copy-this-line)
  (my/g-menu-set "ew" "単語コピー" #'my/copy-this-word)
  (my/g-menu-set "ed" "行削除" #'kill-whole-line)
  (my/g-menu-set "ec" "選択コメント" #'comment-dwim)
  (my/menu-group "em" "マルチカーソル")
  (my/g-menu-set "emr" "リージョン内" #'mc/edit-lines)
  (my/g-menu-set "emd" "dwim" #'mc/mark-all-dwim)
  (my/menu-group "en" "ナロー")
  (my/g-menu-set "enn" "リージョン" #'narrow-to-region)
  (my/g-menu-set "enn" "関数" #'narrow-to-defun)
  (my/g-menu-set "ene" "解除" #'widen)
  (my/g-menu-set "es" "文字サイズ" #'text-scale-adjust)
  (my/g-menu-set "ef" "行折返しon/off" #'toggle-truncate-lines)
  (my/g-menu-set "es" "全選択" #'mark-whole-buffer))

(on
  (my/menu-group "w" "ウィンドウ")
  (my/g-menu-set "wh" "横分割" 
    #'(lambda () (interactive)
        (split-window-below)(other-window 1)))
  (my/g-menu-set "wv" "縦分割"
    #'(lambda () (interactive)
        (split-window-right)(other-window 1)))
  (my/g-menu-set "wd" "カレントウィンドウ削除" #'delete-window)
  (my/g-menu-set "wr" "他ウィンドウ削除" #'delete-other-windows)
  (my/g-menu-set "wZ" "ごみ" #'my/sample))

(on
  (my/menu-group "j" "検索・置換・ジャンプ")
  (my/g-menu-set "ja" "ファイルの先頭へ" #'beginning-of-buffer)
  (my/g-menu-set "je" "ファイルの末尾へ" #'end-of-buffer)
  (my/g-menu-set "jr" "置換" #'query-replace)
  (my/g-menu-set "jx" "正規表現置換" #'query-replace-regexp)
  (my/g-menu-set "jg" "指定行へ" #'goto-line))

(on
  (my/menu-group "i" "挿入")
  (my/g-menu-set "id" "日付" #'my/insert-date-format)
  (my/g-menu-set "if" "ファイル" #'insert-file)
  (my/g-menu-set "iq" "制御文字" #'quoted-insert))

(on
  (my/menu-group "m" "キーボードマクロ")
  (my/g-menu-set "ms" "開始" #'kmacro-start-macro)
  (my/g-menu-set "me" "終了" #'kmacro-end-macro)
  (my/g-menu-set "mm" "マップ" #'kmacro-keymap))

(on
  (my/menu-group "v" "表示")
  (my/g-menu-set "vi" "IMenuList" #'imenu-list-smart-toggle)
  (my/g-menu-set "vt" "VTerm" #'vtx)
  ;;(my/g-set-menu-key "vn" "neotree" 'neotree-toggle)
  ;;(my/g-set-menu-key "vd" "neotree dir" 'neotree-dir)
  ;;(my/g-set-menu-key "ve" "Elscreen List" 'my/ivy-elscreen)
  (my/g-menu-set "vz" "lisp sandbox"
    #'(lambda () (interactive)
        (switch-to-buffer "*lisp sandbox*")
        (lisp-interaction-mode))))

(on
  (my/menu-group "u" "機能")
  (my/menu-group "uw" "非表示文字")
  (my/g-menu-set "uwt" "タブ on/off" #'my/sample)
  (my/g-menu-set "uwr" "改行 on/off" #'my/sample)
  (my/g-menu-set "uws" "スペース on/off" #'my/sample)
  (my/g-menu-set "uwz" "全角スペース on/off" #'my/sample)
  (my/g-menu-set "uwe" "ファイル終端文字 on/off" #'my/sample)
  (my/g-menu-set "ul" "行番号 on/off" #'my/sample)
  (my/menu-group "us" "文字サイズ")
  (my/g-menu-set "usb" "大きく" #'my/sample)
  (my/g-menu-set "use" "標準" #'my/sample)
  (my/g-menu-set "uss" "小さく" #'my/sample)
  (my/g-menu-set "uc" "Compan/y on/off" #'company-mode))

(on 
  (my/menu-group "t" "道具箱")
  (my/g-menu-set "tj" "SKK" #'skk-mode)
  (my/g-menu-set "td" "dired" #'dired)
  (my/g-menu-set "te" "ChangeLog"
    #'(lambda()
        (interactive)
        (add-change-log-entry nil my/changelog-filename))))

(on
  (my/menu-group "r" "記録")
  (my/g-menu-set "rd" "俺の日記"
    #'(lambda () (interactive) (org-capture nil "d")))
  (my/g-menu-set "rg" "今日のぼやき"
    #'(lambda () (interactive) (org-capture nil "g"))))
(on (defun boyaki() (interactive) (org-capture nil "g")))

(on
  (my/menu-group "x" "Shell")
  (my/g-menu-set "xd" "mozc辞書登録"
    #'(lambda () (interactive)(!exec "mozcword &"))))

(on
  (autoload 'yas-global-mode "yasnippet" nil t)
  (autoload 'yas-new-snippet "yasnippet" nil t)
  (autoload 'yas-describe-tables "yasnippet" nil t)
  (autoload 'yas-visit-snippet-file "yasnippet" nil t)
  (autoload 'yas-reload-all "yasnippet" nil t)
  (my/menu-group "y" "yasnippet")
  (my/g-menu-set "ys" "開始" #'yas-global-mode)
  (my/g-menu-set "yn" "新規" #'yas-new-snippet)
  (my/g-menu-set "yl" "一覧" #'yas-describe-tables)
  (my/g-menu-set "yv" "編集" #'yas-visit-snippet-file)
  (my/g-menu-set "yr" "再読込" #'yas-reload-all))

(on
  (my/menu-group "c" "コンパイル")
  (my/menu-group "cl" "Lispコンパイル")
  (my/g-menu-set "cld" "一括コンパイル" #'byte-recompile-directory)
  (my/menu-set emacs-lisp-mode-map "cc" "今のバッファコンパイル"
    #'(lambda () (interactive)(byte-compile-file buffer-file-name)))
  (my/g-menu-set "cq" "サンプル" #'my/sample))

(on
  (my/menu-group "o" "org-mode")
  (my/g-menu-set "oc" "capture" #'counsel-org-capture)
  (my/g-menu-set "oa" "agenda" #'org-agenda)
  ;;(my/g-set-menu-key "of" "キーワード検索" #'my/org-title-list-have-keyword)
  ;;(my/g-set-menu-key "oq" "書籍検索" #'my/org-title-list-have-keyword-book)
  ;;(my/g-set-menu-key "oh" "タイトル一覧" #'my/org-title-list-all)
  )

(on
 (!after 'org

   ;;(my/set-menu-key org-mode-map "ob" "バッファ移動" #'my/org-title-list-buffer-list)
   (my/menu-set org-mode-map "ol" "store link" #'org-store-link)
  (my/menu-set org-mode-map "cc" "org babel コンパイル" #'my/sample)
   (my/menu-set org-mode-map "ow" "copy subtree" #'org-copy-subtree)
   (my/menu-set org-mode-map "on" "narrow toggle"
     #'(lambda()(if(buffer-narrowed-p)(widen)(org-narrow-to-subtree))))
   (my/menu-set org-mode-map "ot" "リンク表示" #'org-toggle-link-display)
   ;;(my/set-menu-key org-mode-map "ox" "装飾" #'my/org-mode-insert-markup-list) ;----
   ;;(my/set-menu-key org-mode-map "oy" "ブロック挿入" #'my/org-mode-insert-block) ;---
   ;;(my/set-menu-key org-mode-map "os" "画面取込" #'my/org-screenshot)
   (my/menu-set org-mode-map "ok" "リンク編集" #'org-insert-link)
   (my/menu-set org-mode-map "oo" "リンク開く" #'org-open-at-point)))

(on
  (autoload 'org-capture-refile "org-capture" nil t)
  (autoload 'org-capture-kill "org-capture" nil t)
  (autoload 'org-capture-finalize "org-capture" nil t)
  (!ewhen (require 'org-capture))
  (!after 'org-capture
    (my/menu-set org-capture-mode-map "ocw" "refile" #'org-capture-refile)
    (my/menu-set org-capture-mode-map "ock" "abort" #'org-capture-kill)
    (my/menu-set org-capture-mode-map "occ" "Finish" #'org-capture-finalize)))

;;;(setq gc-cons-threshold 33554432)
(setq file-name-handler-alist my-saved-file-name-handler-alist)

(profile-proc
  (profiler-report)
  (profiler-stop))
(provide 'my-init)
