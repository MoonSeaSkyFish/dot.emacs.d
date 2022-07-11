;; my-editcommand.el
;; 編集コマンド簡易拡張


(defun my/kill-region (beg end)
  "選択している場合のみカットする"
  (interactive "*r")
  (if mark-active
      (kill-region beg end)
    (message "No Selection")))

(defun my/kill-ring-save (beg end)
  "選択している場合のみコピーする"
  (interactive "*r")
  (if mark-active
      (kill-ring-save beg end) ;beg end や未選択ならカーソル位置の行コピーとかのがよくないか？
    (message "No Selection")))

(defun my/copy-this-line ()
"現在行をコピー"
  (interactive)
  (kill-new (thing-at-point 'line)))

(defun my/copy-this-word ()
  "現在位置のワードをコピー"
  (interactive)
  (kill-new (thing-at-point 'word)))

(provide 'my-editcommand)
