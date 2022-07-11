;;kn 各行に対してなにかをする処理

;試作品

(defun my/line-begin-end-add-str ()
  (interactive)
  (let ((b (read-from-minibuffer "追加行頭: "))
        (e (read-from-minibuffer "追加行末: ")))
    (deactivate-mark)
    (perform-replace "^\\(.*\\)" (concat b "\\1" e)
                     nil t nil nil nil (region-beginning) (- (region-end) 1))))

;;TODO
;; - 正規表現置換
;; - 条件付き置換
;; - 削除バージョンもあると便利？ 置換で/* ... */したら、はずしたいよね？

(provide 'my-line-processor)

