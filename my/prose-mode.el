;;
;; prose-mode.el
;;
;; Create Dated: <2020/01/31 11:58:33>
;; Last Updated: <2021/03/16 23:12:02>
;; 

(define-derived-mode prose-mode org-mode "prose"
  "Major mode for edit prose."
  (define-key prose-mode-map (kbd "<return>") 'prose-return)
  )

;; 改行時、括弧の前のスペース除去、次の行の行頭にスペースを入れる
(defun prose-return ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^　[「（]" nil t)
        (progn
          (beginning-of-line)
          (delete-char 1)))
    (if (re-search-forward "　+$" nil t)
        (replace-match "")))
  (org-return)
  (insert "　"))

(defun prose-kaiwa-kakko ()
  (interactive)
  (message "kakko!!")
  )

(add-to-list 'auto-mode-alist '("\\.prose\\'" . prose-mode))
(add-to-list 'auto-mode-alist '("\\.prose\\.txt\\'" . prose-mode))
;; 電撃 43x34
;; ミステリーズ新人賞 40x40
;; オール読み物 40x30
;; メフィスト 40x40
;; 乱歩 30x40
;; ホラー 40x40
;;

(provide 'prose-mode)
;; end









