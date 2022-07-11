;;
;; my-vertico-extend.el
;;
;;  vertico拡張
;;
;; Create Dated: <2020/06/21 21:47:20>
;; Last Updated: <2022/07/10 16:27:46>
;;
(require 'my-lib)
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 日時挿入
(defvar my/rsrc-date-format
      '(
        "%Y-%m-%d %H:%M:%S"
        "%Y-%m-%dT%H:%M:%S+09:00"
        "%Y年%m月%d日 %H時%M分"
        "Create Dated: <%Y/%m/%d %H:%M:%S>"
        "Last Updated: <2022/07/10 16:27:46>"
        ))

(defun my/insert-date-format()
  (interactive)
  (insert
    (completing-read "date: " (mapcar 'format-time-string my/rsrc-date-format))))


;; 同一名のタイトルは避けること

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
  (let (line f w)
    (setq line 
      (assoc (completing-read "favfile: " (mapcar 'car my/rsrc-fav-file-list))
        my/rsrc-fav-file-list))
    (setq f (cl-second line))
    (setq w (cl-third line))
    (if (file-directory-p f)
      (dired f)
      (my/find-file-jump-keyword f w))))

;; #お気に入りディレクトリ #favdir
(defun my/find-fav-dir()
  (interactive)
  (let (line)
    (setq line
      (assoc
        (completing-read "favdir: " (mapcar 'car my/rsrc-fav-dir-list)) my/rsrc-fav-dir-list))
    (dired (cl-second line))))




;; --- 以下は未実装 ----

;; Template, ファイル新規作成系
;; org-junkとかでも代用可能？



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

(defvar my/org-base-dir "/home/text/org/")
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


;;;;;;
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



;; * テンプレート
;; ** File New Open
;; *** memo(org diary,,,,)
;; *** programing
;; *** blog
;; ** Insert Text
;; ** Org-mode(org-capture)

(provide 'my-vertico-extend)
;; end
