;;
;; functions.el
;;
;; Create Dated: <2018/10/21 00:19:50>
;; Last Updated: <2021/05/29 23:38:27>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                自 作 関 数 集
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 末尾改行空白削除
(defun my/chomp-end (str)
      "Chomp tailing whitespace from STR."
      (replace-regexp-in-string (rx (* (any " \t\n")) eos)
                                ""
                                str))


;; 検索結果をリストにして返す 返り値((filename, lineno, sertch-string)(...)...)_
(defun my/grep (ptn &optional dir ext)
  (let ((result)(cmd (format "rg '%s' -n" ptn)))
    (if (= (length dir) 0)
        (if (null (buffer-file-name))
            (setq dir "~/")
          (setq dir (file-name-directory buffer-file-name))))
    (if (> (length ext) 0)(setq cmd (format "%s -g '%s'" cmd ext)))
    (setq cmd (format "%s %s|sort" cmd dir))
    (setq result (my/chomp-end (shell-command-to-string cmd)))
    (if (> (length result) 0)
        (mapcar #'(lambda(line)
                   (if (string-match "^\\([^:]+\\):\\([^:]+\\):\\(.*\\)$" line)
                       (list (match-string 1 line)(match-string 2 line)(match-string 3 line))))
                (split-string result "\n")))))

;;検索結果でファイル名が同一の場合、それぞれにまとめる
;; キー:ファイル名 値:行数 検索結果の文字列
;; (filename ((lineno . text)(lineno . text)...) ... hashtable利用する
(defun my/grep-ex (ptn &optional dir ext)
  (let (filename no-text (hashresult (make-hash-table :test 'equal))
                 (cmd (format "rg '%s' -n" ptn)))
    (if (= (length dir) 0)
        (if (null (buffer-file-name))
            (setq dir "~/")
          (setq dir (file-name-directory buffer-file-name))))
    (if (> (length ext) 0)(setq cmd (format "%s -g '%s'" cmd ext)))
    (setq cmd (format "%s %s|sort" cmd dir))
    (dolist (line (split-string (shell-command-to-string cmd) "\n"))
      (if (string-match "^\\([^:]+\\):\\([^:]+\\):\\(.*\\)$" line)
          (progn
            (setq filename (match-string 1 line))
            (setq no-text (cons (match-string 2 line) (match-string 3 line)))
          (puthash filename (push no-text (gethash filename hashresult)) hashresult))))
    hashresult))

;;
(defun my/change-text-dir (dir)
  (concat (directory-file-name my/text-dir) "/" (directory-file-name dir) "/"))


;; カーソル位置が文字列か？
(defun my/context-in-string-p ()
  (nth 3 (parse-partial-sexp (point) (point-min))))

 ;; カーソル位置がコメントか？
(defun my/context-in-comment-p ()
  (nth 4 (parse-partial-sexp (point) (point-min))))

;;指定ディレクトリ内の指定拡張子の最大の数字をもつファイル名を返す。拡張子は付加しない。
(defun my/get-filename-max-number (dir ext)
  "指定ディレクトリ内の指定拡張子の最大の数字をもつファイル名を返す。拡張子は付加しない。")

;;指定ディレクトリ内の指定拡張子の最小の数字をもつファイル名を返す。拡張子は付加しない。
(defun my/get-filename-min-number (dir ext)
  "指定ディレクトリ内の指定拡張子の最小の数字をもつファイル名を返す。拡張子は付加しない。")

;;
(defun my/max-fileno (dir ext)
  "ファイル名の形式が数字.拡張子 の一番大きい数字を返す。なければ0を返す"
  (let ((maxid 0)  (last-file nil))
    (dolist (f (directory-files dir nil (format "^[0-9]+\\.%s" ext) nil))
      (if (file-regular-p (expand-file-name f dir))
          (if (string-match "\\([0-9]+\\)" f)
              (let ((id (string-to-number (match-string 1 f))))
                (if (> id maxid)
                    (setq maxid id))))))
    maxid))


;; * orgテキストの#+XXXX: VVVVを取り出す なければnilを返す
;; ありそうなのだが見つからず作成した
;; 別のやりかたがある my-org-libで実装
;; (defun my/org-get-value (key)
;;   "orgテキストの#+key: values のvalueを返す。なければnilを返す"
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (re-search-forward (format "^\s*#\\+%s\s*:\s*\\(.*\\)$" key) (point-max) t)
;;         (match-string 1))))

(defun my/goto-line (no)
  "no行にjump"
  (goto-char (point-min))
  (forward-line (1- no)))


;; * 指定ファイルをオープンして、指定行数に飛ぶ
;; ありそう。探してないからあると思う。でも探すのが面倒で作成した。
(defun my/open-file-lineno (f no)
  "引数:ファイル、行数 指定したファイルを開いて指定行数に飛ぶ。"
    (find-file f)
    (my/goto-line no))

;; * 指定ファイルをオープンして、指定の検索文字列に飛ぶ
;; ありそう。探してないからあると思う。でも探すのが面倒で作成した。
(defun my/find-file-jump-keyword (f w)
  "引数:ファイル、検索文字列 指定したファイルを開いて検索文字列に飛ぶ。なければ行頭。"
    (find-file f)
    (search-forward w nil t))


;; lisp-interaction-modeでのみバッファをクリアにする
(defun cls ()
  "lisp-interaction-modeでのみバッファをクリア"
  (interactive)
  (if (equal major-mode 'lisp-interaction-mode) (erase-buffer)))

;; カレントバッファをバイトコンパイルする
(defun my/byte-compile-buffer ()
  "カレントバッファをバイトコンパイルする"
  (interactive)
  (byte-compile-file (buffer-file-name)))

;; emacsの起動数を取得(厳密じゃないかも)
(defun my/count-emacs ()
  "enacsの起動数を取得。"
  (string-to-number (shell-command-to-string "ps|grep emacs|wc -l")))

;; カーソル行の文字数を返す
(defun my/count-line-chars ()
  "カーソル行の文字数を返す"
  (- (point-at-eol) (point-at-bol)))

;; 文字数と列数から行数を算出する
(defun my/count-lines-of-len (len cols)
  "文字数と列数から行数を算出。引数：文字数、列数"
  (ceiling (/ (float len) cols)))

;; カレントバッファの情報を表示。
(defun my/buffer-info ()
  "カレントバッファの情報を表示。
ファイル名、バッファ名、メージャーモード、マイナーモード、文字コード、改行コード、
ファイルサイズ、更新日時、変更の有無"
  (interactive)
  (let ((bn "* buffer infomation *"))
    (with-output-to-temp-buffer bn
      (princ (format "Buffer Name    : %s\n" (buffer-name)))
      (princ (format "Buffer Modified: %s\n" (if (buffer-modified-p) "** Modified" "--")))
      (princ (format "File Name      : %s\n" (buffer-file-name)))
      (princ (format "Encoding       : %s\n" buffer-file-coding-system))
      (princ (format "Buffer Size    : %s\n" (buffer-size)))
      (if (buffer-file-name)
          (princ (format-time-string "Last Updated   : %Y-%m-%d %H:%M:%S\n"
                                     (visited-file-modtime))))
      (princ (format "Line Column    : %dL/%dL %dC/%dC\n"
                     (line-number-at-pos)
                     (count-lines (point-max) (point-min))
                     (current-column)
                     (my/count-line-chars)))
      (princ (format "Major Mode     : %s\n" major-mode))
      (princ "Minor Mode     : ")
      (mapc #'(lambda (m)
                 (if (eval (car m))
                     (princ (format "%s\n                 " (car m))))
                 ) minor-mode-alist))
    (switch-to-buffer-other-window bn)))

;; タイトルバーのテキストを設定
(defun my/set-title-bar-text (txt)
  "タイトルバーのテキストを設定"
  (interactive "sinput: ")
  (setq frame-title-format txt))

;;C-xCv /sudo::
(defun my/reopen-with-sudo ()
  "Reopen current buffer-file with sudo using tramp."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (find-alternate-file (concat "/sudo::" file-name))
      (error "Cannot get a file name"))))


;; Perl ワンライナーを実行
;; いけるとは思うがエラー発生時の処理が問題だ
(defun my/perldo (cmd)
  "perl one liner do"
  (interactive "sPerldo: ")
  (my/region-exec-command (format "perl -npe '%s'" (my/escape cmd))))

;; command を実行しその結果を出力
(defun my/cmddo (cmd)
  "execute input linux command"
  (interactive "sCommand: ")
  (my/region-exec-command cmd))

;; (shell-command-on-region START END COMMAND &optional OUTPUT-BUFFER
;; REPLACE ERROR-BUFFER DISPLAY-ERROR-BUFFER)


(defun my/mark-whole-buffer ()
  "すべてリージョンにする"
  (set-mark (point-min))
  (goto-char (point-max)))

(defun my/region-exec-command (cmd)
  "引数のコマンドを実行し、リージョンのテキストを渡す。リージョンないときはすべて対象。"
  (save-excursion
   (if (not (region-active-p))
       (my/mark-whole-buffer))
   (shell-command-on-region (region-beginning) (region-end)
                            cmd nil t)))
;; エスープ処理
(defun my/escape (s)
  "escape string"
  (replace-regexp-in-string "'" "\\x27" s))

;; 正規表現置換
(defun my/replace(regexp to-string)
  "正規表現置換"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match to-string))))

;;
;; マクロ
;; clロードした場合は不要
;; (defmacro first (x) (list 'car x))
;; (defmacro second (x) (list 'car (list 'cdr x)))
;; (defmacro third (x) (list 'car (list 'cdr (list 'cdr x))))
;; (defmacro fourth (x) (list 'car (list 'cdr (list 'cdr (list 'cdr x)))))
;; (defmacro fifth (x) (list 'car (list 'cdr (list 'cdr (list 'cdr (list 'cdr x))))))
;; (defmacro sixth (x) (list 'car (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr x)))))))
;; (defmacro seventh (x) (list 'car (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr x))))))))
;; (defmacro eighth (x) (list 'car (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr x)))))))))
;; (defmacro ninth (x) (list 'car (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr x))))))))))
;; (defmacro tenth (x) (list 'car (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr (list 'cdr x)))))))))))

;;;;
;; #buffer #next-buffer #prev-buffer
;; next-buffer,previous-bufferでの移動で
;; 作業系バッファには移動しないようにする
;;
(defun my/asterisked? (buf-name)
  (= 42 (car (string-to-list buf-name))))

(defun my/mv-buffer-target? (buf-name)
  (cond
   ;;((string= buf-name "*scratch*") nil)
   ;; 移動バッファの対象が増えたらここに付け足す
   ((my/asterisked? buf-name) t)
   (t nil)))

(defun my/move-to-scratch ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while (and (not (string= "*scratch*" (buffer-name)))
                (not (string= current-buffer-name (buffer-name))))
      (next-buffer))))

(defun my/next-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while (and (my/mv-buffer-target? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (next-buffer))))

(defun my/previous-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (previous-buffer)
    (while (and (my/mv-buffer-target? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (previous-buffer))))


;;; 指定文字数の幅に設定する
(defun my/set-column (column)
  (interactive "nColumn: ")
  (let ((m (- (frame-width) column))
	(r 0) (l 0))
    (if (> m 0)
	(progn
	  (setq r (+ (/ m 2) (% m 2)))
	  (setq l (/ m 2)))
      (progn
	(set-frame-size (selected-frame) column (frame-height))))
    (set-window-margins (selected-window) r l)))

;;; 指定文字数の幅に設定する(全角文字数設定)
(defun my/set-column-jp (column)
  (interactive "nColumn: ")
  (my/set-column (* 2 column)))

;; #TODO
;;開いているバッファで変更してないものを全部閉じる
(defun my/close-all-buffers ()
   (interactive)
   (dolist (b (buffer-list))
     (if (and (buffer-file-name) b (not (buffer-modified-p b)))
         (kill-buffer b))))
 

;;ゼロ幅スペース
(defvar my/zero-width-space #x200b)

;; 現在行をコピー
(defun my/copy-this-line ()
  (interactive)
  (kill-new (thing-at-point 'line)))

;; 現在位置のワードをコピー
(defun my/copy-this-word ()
  (interactive)
  (kill-new (thing-at-point 'word)))

;;ゼロ幅スペースの入力
(defun my/insert-zero-width-space ()
  (interactive)
  (insert-char my/zero-width-space))
  

;; 現在行をカット
;; 現在位置のワードをカット
;; 現在行を削除
(defun my/delete-this-line ()
  (interactive)
  (kill-new (thing-at-point 'line)))
;; 現在位置のワードを削除


;; 指定メジャーモードのバッファ一覧
(defun my/buffer-list-major-mode (majormode)
  (let (lst)
    (dolist (b (buffer-list))
      (if (eq (cdr (assoc 'major-mode (buffer-local-variables b))) majormode)
          (push b lst)))
    lst))

(provide 'my-lib)

;; end
