;;
;; my-org-lib.el
;;
;; Create Dated: <2020/06/26 01:52:48>
;; Last Updated: <2021/10/23 07:14:52>
;;

(require 'my-lib)


;;
(defun* my/org-get-keywords-in-dir ())
(defun* my/org-get-keywords-in-diXr (dir &optional (extptn "*.org"))
  "指定ディレクトリ以下のorgファイルのキーワードのリストを返す。
ハッシュ配列として返却する。grepしてキーワード所得している。
(filename ((key (value . lineno) (value . lineno) ....))かな？
"
  (let ((ht-grep (my/grep-ex "^#\\+.+:" dir extptn))
        (ht-result (make-hash-table :test 'equal)))
    (maphash
     '(lambda (filename no-text-list)
        (let (text-key text-value kv-list
                       (ht-kv (make-hash-table :test 'equal)))
           (dolist (no-text no-text-list)
             (let ((lineno (car no-text))
                   (text (cdr no-text)))
               (if (string-match "^#\\+\\([^:]+\\): *\\(.*\\) *$" text)
                   (progn
                     (setq text-key   (match-string 1 text))
                     (setq text-value (match-string 2 text))
                     (setq kv-list (gethash text-key ht-kv))
                     (push (cons text-value lineno) kv-list)
                     (puthash text-key kv-list ht-kv)))))
           (puthash filename ht-kv ht-result)))
      ht-grep) ;; filename (lineno text)...text="#+key:value"
    ht-result))



;; ;;(defun* my/org-get-keyword-list-in-dir (dir &optional (extptn "*.org"))
;; (defun* XXmy/org-get-keywords-in-dir (dir &optional (extptn "*.org"))
;;   "指定ディレクトリ以下のorgファイルのキーワードのリストを返す。
;; ハッシュ配列として返却する。grepしてキーワード所得している。
;; (filename ((key (value lineno) (value lineno) ....))かな？
;; "
;;   (let ((ht-grep (my/grep-ex "^#\\+.+:" dir extptn))
;;         (ht-result (make-hash-table :test 'equal)))
;;     (maphash
;;      '(lambda (filename no-text-list)
;;         (let ((ht-kv (make-hash-table :test 'equal)))
;;           (dolist (no-text no-text-list)
;;             (let ((lineno (car no-text))
;;                   (text (cdr no-text))
;;                   text-key text-value kv-list)
;;               (if (string-match "^#\\+\\([^:]+\\): *\\(.*\\) *$" text)
;;                   (progn
;;                     (setq text-key (match-string 1 text))
;;                     (setq text-value (match-string 2 text))
;;                     (setq kv-list (gethash text-key ht-kv))
;;                     (push (cons text-value lineno) kv-list)
;;                     (puthash text-key kv-list ht-kv)))))
;;           (puthash filename ht-kv ht-result)))
;;      ht-grep) ;; filename (lineno text)...text="#+key:value"
;;     ht-result))

(defun my/org-get-keyword-all ()
  "カレントバッファのキーワードのリストを返す(key . value)
ref:http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key keyword)
                                           (org-element-property :value keyword)))))

(defun my/org-get-keyword-value (key)
  "カレントバッファの指定キーの値を取得。
同じキーワードで複数ある場合があるため、リストで返す" ;;もっとよいやり方ないだろうか？
  (let (lst)
    (dolist (v (my/org-get-keyword-all))
      (if (equal (car v) key)
          (push (cdr v) lst)))
    lst))

;;このあたりは、すでに実装されてそうではある。
(defun my/org-get-context-at-point(elm &optional isall)
  "カーソル位置のコンテキストの内容を取得する。
現在位置のelementが違う場合はnil
引数isallがtの場合はすべて、nilの場合はコンテンツのみ取得。"
  (let (beg end (context (org-element-context)))
    (if (eq (org-element-type context) elm)
        (progn
          (if isall
              (progn
                (setq beg (org-element-property :begin context))
                (setq end (org-element-property :end context)))
            (progn
              (setq beg (org-element-property :contents-begin context))
              (setq end (org-element-property :contents-end context))))
          (if (and beg end)
              (buffer-substring-no-properties beg end)
            nil)))))

(defun my/org-delete-context-at-point(elm &optional isall)
  "カーソル位置のコンテキストのを削除する。
引数がtの場合はすべて、nilの場合はコンテンツのみ削除。"
  (let (beg end (context (org-element-context)))
    (if (eq (org-element-type context) elm)
        (progn
          (if isall
              (progn
                (setq beg (org-element-property :begin context))
                (setq end (org-element-property :end context)))
            (progn
              (setq beg (org-element-property :contents-begin context))
              (setq end (org-element-property :contents-end context))))
          (if (and beg end)
              (delete-region beg end)
            nil)))))

(defun my/org-get-description-at-point ()
  "現在位置のリンクのDescription取得"
  (my/org-get-context-at-point 'link))

(defun my/org-get-link-at-point ()
  "現在位置のリンクを取得"
  (my/org-get-context-at-point 'link t))

(defun my/org-delete-link-at-point ()
  "現在位置のリンクを削除"
  ;(setq BBB "start")
  (my/org-delete-context-at-point 'link t))

;;とりあえず
(defun my/org-get-headline-contents()
  "カーソル位置のコンテンツを返す" ;;narrow-subtree利用せずやりたいが...
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (mark-whole-buffer)
      (buffer-substring-no-properties
       (region-beginning) (region-end))))
  (keyboard-quit))


(defun my/org-babel-byte-compile (file)
  "org bable ファイルからソースを取り出してバイトコンパイルする(elisp用)"
  (interactive "forg-file:")
  (let ((elname (concat (file-name-sans-extension file) ".el")))
    (org-babel-tangle-file file elname "emacs-lisp")
    (byte-compile-file elname)))


(defun my/recent-org-file (dir max)
  "指定ディレクトリの最近更新したorgファイルの一覧のタイトルを返す。"
  (let ((lst) (db (my/org-get-keywords-in-dir dir)))
    (maphash
       '(lambda (filename ht)
          (push (list  filename
                       (car (car (gethash "TITLE" ht)))
                       (car (car (gethash "LAST" ht))))
                lst)) db)
    (setq lst (sort lst '(lambda (a b) (string> (cl-third a) (cl-third b)))))
  (message "run my recent org s?%s " (length (cl-subseq lst 0 max)))
    (cl-subseq lst 0 max)))

(defun my/org-targetdir (mode defalt-dir)
  "バッファが指定のmodeならバッファのディレクトリ。違えばdefault-dirを返す"
  )

(provide 'my-org-lib)

;; end
