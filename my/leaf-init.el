;;; init.el --- Emacs configuration file. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015-2022 Sorao Tsukiumi
;;

;(require 'profiler)
;(profiler-start 'cpu)

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
;;
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  ;;(package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; ----------------------------------------------------------------------
;; 各種設定
;; ----------------------------------------------------------------------
(leaf 初期設定
  :config
  (leaf お気に入り
    :config
    (defvar my/rsrc-fav-file-list
      '(("init.el" "~/.emacs.d/init.el")
        ("edit favfile" "~/.emacs.d/init.el" "my/rsrc-fav-file-list")
        ("openbox menu.xml" "~/.config/openbox/menu.xml")
        ("openbox rc.xml" "~/.config/openbox/rc.xml")
        ("openbox autostart" "~/.config/openbox/autostart")))
    (defvar my/rsrc-fav-dir-list
      '(("pg" "/home/pg/")
        (".emacs.d" "~/.emacs.d/")
        ("Templates" "~/Templates"))))

  (leaf ロードパス設定
    :config
    (add-to-list 'load-path "~/.emacs.d/personal/")
    (add-to-list 'load-path "~/.emacs.d/lisp/"))
  
  (leaf GC関連
    :config
    (setq gc-cons-threshold (* 800000 50))
    (setq garbage-collection-messages t))

  (leaf server
    :require nil
    :commands (server-running-p)
    :config
    (unless (server-running-p) (server-start)))

  (leaf cus-start
    :custom
    '((user-full-name . "Sorao Tsukiumi")
      (user-mail-address . "moon.sea.sky.fish@gmail.com")
      (inhibit-startup-message . t)
      (bidi-display-reordering . nil)
      (initial-scratch-message . ";; --- scratch ---\n")
      (ring-bell-function . 'ignore)
      (tab-width . 2)
      (indent-tabs-mode . nil)
      (echo-keystrokes .  0.1)
      (scroll-conservatively . 32)
      (scroll-step . 1)
      (scroll-margin . 0))
    :config
    (defun display-startup-echo-area-message ()
      (message "")))

  (leaf cus-edit
    :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

  (leaf その他hooks
    :config
    (add-hook 'find-file-hook #'(lambda () (linum-mode 1)))))


(leaf 外観
  :config
  (leaf 表示
    :config
    (fringe-mode (cons 10 3))
    (setq-default indicate-empty-lines t)
    (setq-default mode-line-format 
                  (list mode-line-mule-info mode-line-modified " %b " "[%l:%C] " mode-line-modes)))

  (leaf highlight-indent-guides
    :doc "indent guide"
    :ensure t
    :require nil
    :custom
    ((highlight-indent-guides-bitmap-function . 'my/highlight-indent-guides--bitmap-line)
     (highlight-indent-guides-method . 'bitmap))
    :config
    (defun my/highlight-indent-guides--bitmap-line (width height _crep zrep)
      (let* ((left (/ (- width 2) 2))
             (right (- width left 2))
             (row (append (make-list left zrep)
                          (make-list 1 " 10000 25535 25535") ;; rgb 0-65535
                          (make-list right zrep))) rows)
        (dotimes (_i height rows)
          (setq rows (cons row rows)))))
    (add-hook 'nim-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'emacs-lisp-mode-hook 'highlight-indent-guides-mode))

  (leaf カーソルについて
    :config
    (add-to-list 'default-frame-alist '(cursor-type . bar))
    (add-to-list 'default-frame-alist '(cursor-color . "#c0c0c0"))
    (add-to-list 'default-frame-alist '(mouse-color . "#ff0000"))
    (custom-set-faces '(hl-line ((t (:background "#111133")))))
    (global-hl-line-mode t))

  (leaf カラー
    :config
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

  (leaf whitespace
    :require t
    :custom
    ((whitespace-style . '(face tabs tab-mark space-before-tab))
     (whitespace-display-mappings . '((tab-mark   ?\t   [?\x21E5 ?\t] [?\\ ?\t]))))
    :config
    (set-face-foreground 'whitespace-tab "#007777")
    (set-face-background 'whitespace-tab nil)
    (global-whitespace-mode 1))

  (leaf rainbow-delimiters
    :ensure t
    :config
    (defun my/init-rainbow-demiliters()
      (rainbow-delimiters-mode)
      (set-face-foreground 'rainbow-delimiters-depth-1-face "#FFAAAA")
      (set-face-foreground 'rainbow-delimiters-depth-2-face "#00DD00")
      (set-face-foreground 'rainbow-delimiters-depth-3-face "#FF3333")
      (set-face-foreground 'rainbow-delimiters-depth-4-face "#FFFF00")
      (set-face-foreground 'rainbow-delimiters-depth-5-face "#00FFFF")
      (set-face-foreground 'rainbow-delimiters-depth-6-face "#FF00FF")
      (set-face-foreground 'rainbow-delimiters-depth-7-face "#0000FF")
      (set-face-foreground 'rainbow-delimiters-depth-8-face "#99EE88")
      (set-face-foreground 'rainbow-delimiters-depth-9-face "#9999FF"))
    (add-hook 'emacs-lisp-mode-hook 'my/init-rainbow-demiliters) 
    (add-hook 'lisp-interaction-mode-hook 'my/init-rainbow-demiliters)))
  

(leaf leaf関連
  :config
  (leaf leaf-tree
    :ensure t
    :custom
    ((imenu-list-size . 30)
     (imenu-list-position . 'left))
    :config
    (add-hook 'find-file-hook ;;; init.el を開いたときに leaf-tree を表示する
              #'(lambda() (if (string-match "/init.el$" (buffer-file-name))
                             (leaf-tree-mode))))))

(leaf ファイル保存関連
  :config
  (leaf バックアップとか
    :config
    (setq backup-directory-alist
     (cons (cons ".*" (expand-file-name "~/big/.backup/emacs"))
           backup-directory-alist))
    (setq auto-save-list-file-prefix
          "~/big/.backup/emacs/auto-save-list")
    (setq auto-save-file-name-transforms
          `((".*", (expand-file-name "~/big/.backup/emacs") t))))

  (leaf my-timestamp
    :require nil
    :config
    (add-hook 'before-save-hook 'my/time-stamp))

  (leaf recentf
    :disabled nil
    :custom
    ((recentf-max-saved-items . 1000)
     (recentf-exclude . '("recentf\\'" "\\.org\\'" ".+\\.owl\\'" )))
    :commands recentf-mode
    :config
    (recentf-mode))
  
  (leaf magit :ensure t))

(leaf 日本語関連
  :config
  (set-language-environment "Japanese")
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)

  (leaf mozc
    :disabled nil
    :ensure t
    :config
    (setq default-input-method "japanese-mozc")
    (setq mozc-candidate-style 'overlay)
    (leaf mozc-cand-posframe
      :require t
      :after posframe
      :config
      (setq mozc-candidate-style 'posframe)
      (set-face-attribute 'mozc-cand-posframe-normal-face nil
                          :foreground "#ffeeff"
                          :background "#335577")
      (set-face-attribute 'mozc-cand-posframe-focused-face nil
                          :foreground "#335577"
                          :background "#ccffcc")
     
      (set-face-attribute 'mozc-cand-posframe-footer-face nil
                          :foreground "#ffeeff"
                          :background "#335577")))
  
  (leaf migemo
    :ensure t
    :commands migemo-init
    :config
    (setq migemo-command "/usr/bin/cmigemo") ; HERE cmigemoバイナリ
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/share/migemo/utf-8/migemo-dict") ; HERE Migemo辞書
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    :init
    (migemo-init)))


(leaf ユーティリティ
  :config
  (leaf vterm
    :disabled nil
    :ensure t
    :custom
    ((vterm-max-scrollback . 10000))
    :config
    (set-face-foreground 'vterm-color-black   "#2e3436")  ;; 0 - ?
    (set-face-foreground 'vterm-color-red     "#aabbff")  ;; 1
    (set-face-foreground 'vterm-color-green   "#4e9a06")  ;; 2 - exe
    (set-face-foreground 'vterm-color-yellow  "#c4a000")  ;; 3
    (set-face-foreground 'vterm-color-blue    "#3465A4")  ;; 4 - directory
    (set-face-foreground 'vterm-color-magenta "#75507B")  ;; 5
    (set-face-foreground 'vterm-color-cyan    "#ce5c00")  ;; 6
    (set-face-foreground 'vterm-color-white   "#babdb9")  ;; 7
    (set-face-background 'vterm-color-black   "#555753")  ;; 8
    (set-face-background 'vterm-color-red     "#EF2929")  ;;9
    (set-face-background 'vterm-color-green   "#8AE234")  ;;10
    (set-face-background 'vterm-color-yellow  "#FCE94F")  ;;11
    (set-face-background 'vterm-color-blue    "#729FCF")  ;;12
    (set-face-background 'vterm-color-magenta "#AD7FA8")  ;;13
    (set-face-background 'vterm-color-cyan    "#fcaf3e")  ;;14
    (set-face-background 'vterm-color-white   "#EEEEEC")  ;;15
    (defun vtx () (interactive) (vterm-other-window))
    (defun my/vterm-sendline (str)
      (vterm-send-string (concat str "\n")))
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
    :bind
    ((vterm-mode-map
      ("C-b" . switch-to-buffer)
      ("C-w" . other-window))))
    (setq dummy-line nil))

(leaf 操作支援
  :config
  (leaf which-key
    :ensure t
    :init
    (which-key-mode)
    :custom
    ((which-key-separator . ":" )
     (which-key-prefix-prefix . "" )
     (which-key-idle-delay . 0.1)
     (which-key-idle-secondary-delay . 0.1)
     (which-key-max-display-columns . 1))
    :config
    (leaf  which-key-posframe
      :ensure t
      :require t
      :custom
      ((which-key-posframe-poshandler . 'posframe-poshandler-point-bottom-left-corner))
      :init
      (which-key-posframe-mode)))
  
  (leaf delsel
    :doc "delete selection if you insert"
    :tag "builtin"
    :global-minor-mode delete-selection-mode)

  (leaf posframe :ensure t :require t)
  (leaf multiple-cursors :ensure t)
  (leaf treemacs :ensure t)
  (leaf expand-region :ensure t))

(leaf 入力支援
  :config
  ;;(electric-pair-mode 1)
  
  (leaf smartparens :ensure t
    :init
    (leaf smartparens-config :require t)
    (smartparens-global-mode 1)
    :config
    (sp-pair "#[" "]#")
    (sp-pair "\"\"\"" "\"\"\""))
  
  (leaf my-line-processor :require t)  

  (leaf reformatter
    :ensure t
    :config
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
      :lighter " DF"))
  
  (leaf paren
    :config
    (set-face-attribute 'show-paren-match nil :background "#333333"
                        :foreground nil :underline t :bold t
                        :inverse-video nil)
    (show-paren-mode t))

   (leaf vertico
    :disabled t
    :ensure t
    :custom
    (vertico-count . 20)
    :config
    (vertico-mode))

   (leaf ivy
    :disabled nil
    :ensure t
    :custom
    ((ivy-use-virtual-buffers . t)
     (ivy-count-format . "(%d/%d) "))
    :config
    (leaf swiper
      :ensure t
      :config
      (leaf counsel :ensure t))
    (leaf my-ivy-extend :require t)
    :init
    (ivy-mode 1) ;; recentf load suru
    )
 
  (leaf company
    :ensure t
    :defvar company-backends
    :custom
    ((company-idle-delay . 0) ; デフォルトは0.5
     (company-minimum-prefix-length . 3) ; デフォルトは4
     (company-selection-wrap-around . t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
     (company-transformers . '(company-sort-by-occurrence))
     ;; ↑sort company-sort-by-occurrence / company-occurrence-weight-function /
     ;;        company-sort-by-backend-importance / company-sort-prefer-same-case-prefix
     (company-dabbrev-downcase . nil)) ;;小文字にしない
    :require t
    :init
    (global-company-mode)
    :config
    ;(add-to-list 'company-backends 'company-yasnippet)
    
    (leaf company-posframe
      :ensure t
      :require t
      :init
      (company-posframe-mode 1))

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
                        :background "gray40"))

  (leaf yasnippet
    :ensure t
    :require t
    :custom
    ((yas-snippet-dirs .
      '("~/.emacs.d/mysnippets"
        "~/.emacs.d/snippets"))
     (yas-also-auto-indent-first-line . t)
     )
    ;:init
    ;(yas-global-mode 0) ;; timer?
    )
  
  (leaf ivy-yasnippet
    :ensure t
    :after (yasnipetts ivy)
    ))
  
(leaf プログラミング
  :config
  (leaf lsp-mode :ensure t)
  
  (leaf paredit
    ;;キーバインド等は考慮必要
    :ensure t
    :config
    (define-key paredit-mode-map (kbd "C-j") nil)
    :hook
     ((emacs-lisp-mode-hook . enable-paredit-mode)))

  (leaf web-mode
    :require t
    :mode
    (("\\.phtml\\'" . web-mode)
     ("\\.tpl\\.php\\'" . web-mode)
     ("\\.php\\'" . web-mode)
     ("\\.[gj]sp\\'" . web-mode)
     ("\\.as[cp]x\\'" . web-mode)
     ("\\.erb\\'" . web-mode)
     ("\\.mustache\\'" . web-mode)
     ("\\.djhtml\\'" . web-mode)
     ("\\.html?\\'" . web-mode)
     ("\\.html.ep\\'" . web-mode))
    :custom
    ((web-mode-engines-alist . '(("php" . "\\.php\\'")("blade" . "\\.blade\\."))))
    :config
    ;;; インデント数
    (add-hook 'web-mode-hook
              #'(lambda()
                  (setq web-mode-markup-indent-offset 2)
                  (setq web-mode-code-indent-offset 2)
                  (setq web-mode-css-indent-offset 2))))

  (leaf perl
    :config
    (autoload 'cperl-mode "cperl-mode" "For editing Perl script" t)
    (defalias 'perl-mode 'cperl-mode))

  (leaf typescript-mode
    :ensure t
    :config
    (my/vterm-cd-cmd default-directory buffer-file-name)
    :hook
    ((typescript-mode-hook . lsp-deferred)
     (typescript-mode-hook . ts-format-on-save-mode))
    :custom
    ((typescript-indent-level . 2))
    :mode
    (("\\.ts\\'" . typescript-mode)
     ("\\.tsx\\'" . typescript-mode)))

  (leaf sass-mode :ensure t)
  
  (leaf nim-mode
    :ensure t
    :config
    (defun my/find-nimblefile (pdir) ;;上ディレクトリに向かってxxx.nimble探す
      (let ((dir (file-name-as-directory pdir))
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
    (defun my/nim-compile ()
      (interactive)
      (my/vterm-cd-cmd default-directory (concat "nim c -r " buffer-file-name)))
    (defun my/nim-build ()
      (interactive)
      (let ((dir (my/find-nimblefile default-directory)))
        (if dir
            (my/vterm-cd-cmd dir "nimble build")
          (message "not found nimblefile."))))
    (my/set-menu-key nim-mode-map "cc" "nim c -r    " 'my/nim-compile)
    (my/set-menu-key nim-mode-map "cb" "nimble build" 'my/nim-build)
    :bind
    ((nim-mode-map
      ("<f5>" . 'my/nim-compile)
      ("<f6>" . 'my/nim-build)))
    :hook
    ((nim-mode-hook . lsp)
     (nim-mode-hook . nim-format-on-save-mode))))
  
(leaf org
  :require t
  :custom
  ((org-startup-truncated . nil)
   (org-startup-indented . t)
   (org-level-color-stars-only . nil)
   (org-startup-folded . nil)
   (org-hide-leading-stars . t))
  :config
  (set-face-attribute 'org-level-1 nil  :bold nil :foreground "#b58900")
  (set-face-attribute 'org-level-2 nil  :bold nil :foreground "#dc322f")
  (set-face-attribute 'org-level-3 nil  :bold nil :foreground "#268bd2")
  (set-face-attribute 'org-level-4 nil  :bold nil :foreground "#d33682")
  (set-face-attribute 'org-level-5 nil  :bold nil :foreground "#6c71c4")
  (set-face-attribute 'org-level-6 nil  :bold nil :foreground "#cb4b16")
  (set-face-attribute 'org-level-7 nil  :bold nil :foreground "#2aa198")
  (set-face-attribute 'org-level-8 nil  :bold nil :foreground "#859900")
  (set-face-attribute 'org-block-begin-line nil :bold nil :foreground "#909090")
  (set-face-attribute 'org-block nil    :bold nil :foreground "#aaffee")
  (set-face-attribute 'org-block-end-line   nil :bold nil :foreground "#909090")
  (set-face-attribute 'org-meta-line  nil  :bold nil :foreground "#90aa90")
  (set-face-attribute 'org-document-info  nil  :bold nil :foreground "#90aa90")
  (set-face-attribute 'org-document-info-keyword nil  :bold nil :foreground "#90aa90")
  (set-face-attribute 'org-document-title nil  :bold t :foreground "orange" :height 150)
  (set-face-attribute 'org-table  nil  :bold nil :foreground "#ffccaa")

  (leaf my-ivy-org-capture :require t)
  (leaf my-diary :require t)

  ;; org-capture テンプレ
  (setq org-capture-templates
        '(("d" "おれの日記" entry (file+headline my/diary-org-file "Diary") "** %?")
          ("g" "ぼやき" item (file+headline my/diary-org-file "Grumble")
           (function my/diary-grumble-template))))
  
  (leaf バベルの塔
    :config
    ;(leaf ob-nim)
    )
  
  (leaf org-bullets
    :ensure t
    :custom ((org-bullets-bullet-list . '("✔")))
    :commands org-bullets-mode
    :hook ((org-mode-hook (lambda () (org-bullets-mode 1))))))


;;; ----------------------------------------------------------------------
;;;    key bind
;;; ----------------------------------------------------------------------
(leaf キーバインド
  :config
  (leaf グローバルキー設定
    :config
    ;;Function Key
    (global-set-key (kbd "<f4>") 'my/vterm-cd-bufferdir)
    ;;;---------------------------------------------------
    (global-set-key [henkan] 'toggle-input-method)
    ;; Windows の標準になるべく合わせる
    (leaf my-editcommand
      :require t
      :config
      (keyboard-translate ?\C-x 'control-x) ;;keyboard-translate from to
      (keyboard-translate ?\C-c 'control-c) ;;quoted-insertでは^@と挿入
      :bind
      (([control-x] . my/kill-region)
       ([control-c] . my/kill-ring-save)))

    (leaf expand-region
      :bind
      (("C-=". er/expand-region)))

    (leaf company
      :after yasnippet
      :bind
      (company-active-map
       ("C-f" . company-filter-candidates)
       ;;("SPC" . company-abort)
       ("C-s" . save-buffer)))

    (define-key isearch-mode-map (kbd "C-f")  'isearch-repeat-forward)
    ;;define-key isearch-mode-map (kbd "C-S-f")  isearch-repeat-backward)

    ;; 記号はどうなってるか？ !@#$%^&*()_+=-|\~`'[]{}:;"'/?<>,.
    ;;   →シフト押しながらが多いから使いにくい
    ;; k ... カットとしてpareditで割り振られてるのでカット系で。
    ;; l
    ;; 5 p q  n t y - prefix
    ;; 2 h u - def prefix
    ;; 8 a e m j g d i(tab)
    ;; 4 c x
    ;; 8 v s o f r b w z 
    (global-set-key "\C-v" 'yank)
    (global-set-key "\C-s" 'save-buffer)
    (global-set-key "\C-o" 'find-file)
    (global-set-key "\C-f" 'isearch-forward)
    (global-set-key (kbd "C-S-f") 'isearch-backward)
    (global-set-key "\C-r" 'query-replace)
    (global-set-key "\C-b" 'switch-to-buffer)
    (global-set-key "\C-w" 'other-window)
    (global-set-key "\C-z" 'undo)
    (global-set-key "\C-n" 'ignore)
    (global-set-key "\C-k" 'ignore)
    (global-set-key "\C-l" 'ignore)
    (global-set-key "\C-t" 'ignore)
    (global-set-key "\C-p" 'ignore)
    (global-set-key "\C-y" 'ignore)
    (global-set-key (kbd "C-/") 'ignore)
    (global-set-key "\M-w" 'ignore)

    ;; (global-set-key (kbd "<C-return>")
    ;;                 #'(lambda () (interactive) (end-of-line) (newline)))
    ;; (global-set-key (kbd "<M-return>")
    ;;                 #'(lambda () (interactive) (previous-line) (end-of-line) (newline))))

  (leaf my-newline-indent
    :require t
    :after nim-mode
    :config
     (define-key nim-mode-map (kbd "RET") 'my/nim-newline-and-indent)
     (define-key nim-mode-map (kbd "<C-return>")
       #'(lambda () (interactive) (end-of-line) (my/nim-newline-and-indent)))
     (define-key nim-mode-map (kbd "<M-return>")
       #'(lambda () (interactive) (forward-line -1) (end-of-line) (my/nim-newline-and-indent))))


  ;; p q n t y 削除、コピー、カット、選択 行、単語、行末、行頭
  (leaf プレフィックスキー設定
    :after which-key
    :config
    (global-set-key (kbd "C-l") ctl-x-map) ;;どこにふるか？
    (define-prefix-command 'ctl-p-map)
    (define-prefix-command 'ctl-q-map)
    (define-prefix-command 'ctl-t-map)
    (define-prefix-command 'ctl-n-map)
    (define-prefix-command 'ctl-y-map)
    (global-set-key "\C-p" 'ctl-p-map) ;;
    (global-set-key "\C-q" 'ctl-q-map) ;; その他
    (global-set-key "\C-t" 'ctl-t-map) ;; 削除系
    (global-set-key "\C-n" 'ctl-n-map) ;; カット系
    (global-set-key "\C-y" 'ctl-y-map) ;; コピー系
    (defun my/set-which-key (key desc cmd)
      (global-set-key key cmd)
      (which-key-add-key-based-replacements key desc))
    (defun my/test () (interactive))
    ;; ----- キー設定 -----
    ;; C-y
    (my/set-which-key "\C-yl" "行コピー" 'ignore)
    (my/set-which-key "\C-yw" "単語コピー" 'ignore)
    (my/set-which-key "\C-ya" "行頭までコピー" 'ignore)
    (my/set-which-key "\C-ye" "行末までコピー" 'ignore)
    ;; C-t
    (my/set-which-key "\C-tl" "行削除" 'ignore)
    (my/set-which-key "\C-tw" "単語削除" 'ignore)
    (my/set-which-key "\C-ta" "行頭まで削除" 'ignore)
    (my/set-which-key "\C-te" "行末まで削除" 'ignore)
    ;; C-n
    (my/set-which-key "\C-nl" "行カット" 'kill-whole-line)
    (my/set-which-key "\C-nw" "単語カット" 'kill-word)
    (my/set-which-key "\C-na" "行頭までカット" 'ignore)
    (my/set-which-key "\C-ne" "行末までカット" 'ignore)
    ;; C-p - 各モード用 
    (my/set-which-key "\C-pe" "test" 'my/test)
    ;; C-q
    (my/set-which-key "\C-qe" "test" 'my/test)
    (my/set-which-key "\C-qq" "quoted-insert" 'quoted-insert)))

;; ----------------------------------------------------------------------
;;   Memu
;; ----------------------------------------------------------------------
(leaf 俺メニュー設定
  :after which-key
  :config
  (setq my/menu-key "<muhenkan>") ;; メニューを開くキー
  (defun my/set-menu-key (map key desc cmd)
    (if cmd
        (define-key map (kbd (concat my/menu-key key)) cmd))
    (which-key-add-key-based-replacements (concat my/menu-key key) desc))
  ;;;
  ;;; ---- メニュー表示用マッピング -----
  ;;;
  ;;; ファイル ----------------------------------------
  (my/set-menu-key global-map "f" "ファイル" nil)
  (my/set-menu-key global-map "fs" "保存" 'save-buffer)
  (my/set-menu-key global-map "ff" "開く" 'find-file)
  (my/set-menu-key global-map "fr" "履歴" 'counsel-recentf)
  (my/set-menu-key global-map "fb" "FavFile" 'my/find-fav-file)
  (my/set-menu-key global-map "fd" "FavDir" 'my/find-fav-dir)
  ;;; コンパイル ----------------------------------------
  ;;各種モードマップで定義 cc/cb
  (my/set-menu-key global-map "c" "コンパイル" nil)
  ;;; 記録 ----------------------------------------
  (my/set-menu-key global-map "r" "記録" nil)
  (my/set-menu-key global-map "rd" "俺の日記" '(lambda () (interactive) (org-capture nil "d")))
  (my/set-menu-key global-map "rg" "今日のぼやき" '(lambda () (interactive) (org-capture nil "g")))
  ;;; yasnipett ----------------------------------------
  (my/set-menu-key global-map "y" "yasnippet" nil)
  (my/set-menu-key global-map "yi" "選択・挿入" 'yas-insert-snippet)
  (my/set-menu-key global-map "yn" "新規" 'yas-new-snippet)
  (my/set-menu-key global-map "yl" "一覧" 'yas-describe-tables)
  (my/set-menu-key global-map "yv" "編集" 'yas-visit-snippet-file)
  (my/set-menu-key global-map "yr" "再読込" 'yas-reload-all)
  ;;; org-mode ----------------------------------------
  ;;empty dgijmpruvz
  (my/set-menu-key global-map   "o" "org-mode" nil)
  (my/set-menu-key global-map   "oc" "capture" 'counsel-org-capture)
  (my/set-menu-key org-mode-map "ob" "バッファ移動" 'my/org-title-list-buffer-list)
  (my/set-menu-key global-map   "oa" "agenda" 'org-agenda)
  (my/set-menu-key org-mode-map "ol" "store link" 'org-store-link)
  (my/set-menu-key org-mode-map "ow" "copy subtree" 'org-copy-subtree)
  (my/set-menu-key org-mode-map "on" "narrow toggle" '(lambda()(if(buffer-narrowed-p)(widen)(org-narrow-to-subtree))))
  (my/set-menu-key org-mode-map "ot" "リンク表示" 'org-toggle-link-display)
  (my/set-menu-key org-mode-map "ox" "装飾" 'my/org-mode-insert-markup-list) ;----
  (my/set-menu-key org-mode-map "oy" "ブロック挿入" 'my/org-mode-insert-block) ;---
  (my/set-menu-key org-mode-map "os" "画面取込" 'my/org-screenshot)
  (my/set-menu-key org-mode-map "ok" "リンク編集" 'org-insert-link)
  (my/set-menu-key org-mode-map "oo" "リンク開く" 'org-open-at-point)
  (my/set-menu-key global-map   "of" "キーワード検索" 'my/org-title-list-have-keyword)
  (my/set-menu-key global-map   "oq" "書籍検索" 'my/org-title-list-have-keyword-book)
  (my/set-menu-key global-map   "oh" "タイトル一覧" 'my/org-title-list-all)
  ;;; 編集 ----------------------------------------
  (my/set-menu-key global-map "e" "編集" nil)
  (my/set-menu-key global-map "ec" "行コピー" 'my/copy-this-line)
  (my/set-menu-key global-map "ew" "単語コピー" 'my/copy-this-word)
  (my/set-menu-key global-map "ed" "行削除" 'kill-whole-line)
  ;;; 検索置換 ----------------------------------------
  ;;; キーボードマクロ ----------------------------------------
  (my/set-menu-key global-map "m" "キーボードマクロ" nil)
  (my/set-menu-key global-map "ms" "開始" 'kmacro-start-macro)
  (my/set-menu-key global-map "me" "終了" 'kmacro-end-macro)
  (my/set-menu-key global-map "mm" "マップ" 'kmacro-keymap)
  ;;; 表示 ----------------------------------------
  (my/set-menu-key global-map "v" "表示" nil)
  (my/set-menu-key global-map "vn" "neotree" 'neotree-toggle)
  (my/set-menu-key global-map "vd" "neotree dir" 'neotree-dir)
  (my/set-menu-key global-map "ve" "Elscreen List" 'my/ivy-elscreen)
  ;;; ユーティリティ ----------------------------------------
  (my/set-menu-key global-map"u" "ユーティリティ" nil)
  (my/set-menu-key global-map "ue" "ChangeLog" '(lambda()(interactive)(add-change-log-entry nil my/changelog-filename)))
  ;;; システム ----------------------------------------
  (my/set-menu-key global-map "z" "システム" nil)
  (my/set-menu-key global-map "zq" "Emacs終了" 'save-buffers-kill-terminal))


;;;;;;
;(profiler-report)
;(profiler-stop)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end
