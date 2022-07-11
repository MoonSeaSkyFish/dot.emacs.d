;;
;; my-ivy-extend.el
;;
;;  ivy拡張
;;
;; Create Dated: <2020/06/21 21:47:20>
;; Last Updated: <2022/06/29 00:04:09>
;;
(require 'my-lib)

(defun my-ivy-xxx()
  (interactive)
  (message "ivi"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                  #ivy拡張
;;
;;
;; (ivy-read
;;    "prompt"
;;    Collection
;;    :predicate       (述語)
;;    :require-match
;;    :initial-input
;;    :history
;;    :preselect
;;    :keymap
;;    :update-fn
;;    :sort
;;    :action
;;    :unwind           (展開する)
;;    :re-builder
;;    :matcher
;;    :dynamic-collection
;;    :caller
;;  )
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日時挿入
(defvar my/rsrc-date-format
      '(
        "%Y-%m-%d %H:%M:%S"
        "%Y-%m-%dT%H:%M:%S+09:00"
        "%Y年%m月%d日 %H時%M分"
        "Create Dated: <%Y/%m/%d %H:%M:%S>"
        "Last Updated: <2022/06/29 00:04:09>"
        ))

(defun my/insert-date-format()
  (interactive)
  (ivy-read "date: " (mapcar 'format-time-string my/rsrc-date-format)
    :action '(1 ("o" (lambda (c) (insert c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #お気に入りファイル #favfile
;; jump keywordがあれば、その文字列まで飛ぶ
;; "title"    "file(full path)" "jump keyword"
;;(defvar my/rsrc-fav-file-list
;;      '(
;;        ("俺テンプレ編集" "~/.emacs.d/init.el" "#oretemplete")
;;        ("お気に入りファイル編集" "~/.emacs.d/init.el" "#favfile")
;;        ("Emacs Tips" "~/org/emacstips.org.txt")
;;        ("program" "~/pg/")
;;        ("init.el" "~/.emacs.d/init.el")
;;        ("OpenBox Menu" "~/.config/openbox/menu.xml")
;;        ("OpenBox rc.xml" "~/.config/openbox/rc.xml")
;;        ))

(defun my/find-fav-file()
  (interactive)
  (ivy-read
   "favfile: " my/rsrc-fav-file-list
   :action '(1
             ("o"
              (lambda(c)
                (let ((f (second c)) (w (third c)))
                  (message f)
                  (if (file-directory-p f)
                          (counsel-find-file f)
                    (my/find-file-jump-keyword f w))))
                  "openfile"))))

;; #お気に入りディレクトリ #favdir
(defun my/find-fav-dir()
  (interactive)
  (ivy-read
   "favfile: " my/rsrc-fav-dir-list
   :action '(1
             ("o"
              (lambda(c)
                (dired (second c)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #orgファイルのヘッダー表示
;; ~/org/以下のファイルから、以下の情報を表示
;; #+TITLE:
;; Lev 1 *
;; Lev 2 ** ...
;; Levは、指定可能にする(前置引数とした)
;; ivy拡張として実装
;; grepコマンドは、なにを使うか→agにした。hwはなぜか上手くいかない。
;; grep + perlなんかもいいかもしれない(出力フォーマットが好きにしやすいものねえ)
;; サブディレクトリ内も再帰的に表示する

(defvar my/org-base-dir "~/org/")
(defvar my/org-ext "\\.org\\.txt$")

;;;;;;;;;;;;;;;;;;;

;; ファイルを新規に開いてテンプレートを挿入
;;   - テンプレート選択
;;   - ファイル名入力(省略時は既定ファイル名)
;;  存在しているファイルの場合は、ファイルを開くだけ
(defun my/ivy-open-file-template (template-list)
  (ivy-read
   "Select: " template-list
   :action (lambda (x)
             (let ((prop (cdr x))
                   (new-filename nil)
                   (dir (plist-get prop :dir))
                   (default-filename (plist-get prop :default-filename))
                   (ext (plist-get prop :ext))
                   (template (plist-get prop :template)))
               (setq default-filename (format-time-string default-filename))
               (setq dir (format-time-string dir))
               (setq template (format-time-string template))
               (setq new-filename (read-file-name dir default-filename nil nil))
               (if (string= new-filename dir) (setq new-filename (concat dir default-filename)))
               ;;拡張子が違えば付加する(適当)
               ;;.org.txtのような拡張子にも対応させる為,file-name-extensionは使用しない
               (if (and (not (file-exists-p new-filename))
                        (not (string-match (concat ext "$") new-filename)))
                   (setq new-filename (concat new-filename ext)))
               (find-file new-filename)
               (if (not (file-exists-p (buffer-file-name)))
                   (progn
                     (insert template)
                     ;; %? にカーソルイチを動かす
                     (if (or (re-search-backward "%\\?" nil to)
                             (re-search-forward "%\\?" nil to))
                         (replace-match ""))))))))

;; 選択されたテキストをカーソル位置に挿入
(defun my/ivy-insert-template (template-list)
  (ivy-read
   "Select: " (mapcar 'format-time-string template-list)
   :action (lambda (x)
             (insert x))))

;; 
(defun my/ivy-find-file(file-list)
  (ivy-read
   "Select: " file-list
   :action '(1
             ("o"
              (lambda(c)
                (let ((f (second c)) (w (third c)))
                  (message f)
                  (if (file-directory-p f)
                          (counsel-find-file f)
                    (my/find-file-jump-keyword f w))))
                  "openfile"))))

;; org-capture をivyで利用する
;;  (org-capture nil "?") でいきなりいける
(defun my/ivy-org-capture-template (template-list)
  (ivy-read
   "Select: " template-list
   :action (lambda (x)
             (org-capture nil (car (plist-get (cdr x) :template)))
             )))

;; テンプレートリストをorg-capture-templatesに追加する
(defun my/add-org-capture-templates (template-list)
  (dolist (i (reverse template-list))
    (push (plist-get (cdr i) :template) org-capture-templates)))

;;elscreenのスクリーン一覧を表示、移動
(defun my/ivy-elscreen ()
  (interactive)
  (let ((lst nil))
    (dolist (s (elscreen-get-screen-to-name-alist))
      (push (list (cdr s) (car s)) lst))
    (ivy-read "select: " lst
              :action '(lambda (s) (elscreen-goto (cadr s))))))


;; * テンプレート
;; ** File New Open
;; *** memo(org diary,,,,)
;; *** programing
;; *** blog
;; ** Insert Text
;; ** Org-mode(org-capture)

(provide 'my-ivy-extend)
;; end
