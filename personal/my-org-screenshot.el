;;
;; my-org-screenshot.el
;;
;; Create Dated: <2020/06/26 01:52:48>
;; Last Updated: <2021/01/30 02:12:40>
;;

(defvar my/org-screenshot-command "maim")
(defvar my/org-screenshot-arg "-s")
(defvar my/org-screenshot-save-base-dir "images")
(defvar my/org-screenshot-save-dir  "~/owl")
(defvar my/org-screenshot-extentention ".png")
(defvar my/org-screenshot-fileformat "%Y-%02m-%02d-%02H%02M%02S")
(defvar my/org-filename-number t) ;ファイル名を連番にする

(defun my/org-screenshot-save (filename)
  "スクリーンショットを指定ファイルに保存する"
  (call-process my/org-screenshot-command
                nil nil nil
                my/org-screenshot-arg filename))

(defun* my/org-screenshot-save-wait (filename &optional (sec 2))
  "スクリーンショットをsec秒まって開始する"
  (sleep-for sec)
  (my/org-screenshot-save filename))

(defun my/org-screenshot-make-filename ()
  "スクリーンショットのファイル名を生成する。
場所は、現在のバッファのディレクトリ直下の my/org-screenshot-save-base-dir に保存。
ただしバッファにファイル名がない場合 my/org-screenshot-save-dir に保存。
ファイル名は、バッファにひもづいているファイル名に日時とマイクロ秒を付加。
バッファにファイル名がない場合は、org-macro-templates からファイル名を取得。
なければ接頭辞にnilを付加。
後に日時とマイクロ秒を付加。
my/org-filename-numberがnon-nilなら後に日時ではなく連番を付加。
画像拡張子は、my/org-screenshot-extententionを利用。
"
  (let (base-dir base-filename filename)
    (if (null buffer-file-name)
        (progn ;;バッファにファイル名がない
          (setq base-dir my/org-screenshot-save-dir)
          (if org-macro-templates
              (setq base-filename
                    (file-name-sans-extension
                     (file-name-nondirectory (cdr (assoc "input-file" org-macro-templates)))))))
      (progn
        (setq base-dir (file-name-directory buffer-file-name))
        (setq base-filename (file-name-sans-extension (file-name-nondirectory buffer-file-name)))))
    (if (null base-filename)(setq base-filename "nil"))
    (setq base-dir (concat (file-name-as-directory base-dir)
                           (file-name-as-directory my/org-screenshot-save-base-dir)))
    (if my/org-filename-number
        (let (fn (idx 1))
          (setq fn (format "%s%s_%d%s"
                           (file-name-as-directory base-dir)
                           base-filename idx
                           my/org-screenshot-extentention))
          (while (file-exists-p fn)
            (setq idx (+ idx 1))
            (message (format "%s" fn))
            (setq fn (format "%s%s_%d%s"
                             (file-name-as-directory base-dir)
                             base-filename idx
                             my/org-screenshot-extentention)))
          fn)
      (concat
       (file-name-as-directory base-dir)
       base-filename "_" (format-time-string my/org-screenshot-fileformat)
       "_" (int-to-string (third (current-time))) my/org-screenshot-extentention))))

(defun my/org-screenshot(&optional filename)
  "スクリーンショットを取り、保存、リンクを埋める。
引数にファイル名がある場合、そのファイル名で保存する。
未指定の場合は、my/org-screeshot-make-filename で生成したファイル名を利用する"
  (interactive)
  (if (null filename)
      (setq filename (my/org-screenshot-make-filename)))
  (message "Screenshot OK")
  (my/org-screenshot-save-wait filename)
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images nil t)
  (message "save %s." filename))


(provide 'my-org-screenshot)

;; end
