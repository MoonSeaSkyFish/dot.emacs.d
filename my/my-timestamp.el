;;
;; timestamp.el
;;
;; Copyright (C) 2015-2022 Sorao Tsukiumi
;;

(require 'my-lib)

;; timestamp 最終更新日の挿入や置換
;; concatしてるのは、ここを置換させないため。
(defun my/time-stamp ()
    (interactive)
    (let ((tm (format-time-string "%Y/%02m/%02d %02H:%02M:%02S"))
          (tmorg (format-time-string "%Y-%02m-%02dT%02H:%02M:%02SZ")))
      (my/replace (concat "Create Dated: " "<>") (format "Create Dated: <%s>" tm))
       (my/replace (concat "#\\+date: *$") (format "#+date: %s" tmorg))
       (my/replace (concat "#\\+last: " ".+") (format (concat "#+" "last: %s") tmorg))
       (my/replace (concat "Last Updated: " "<[^>]*>") (format (concat "Last Updated: " "<%s>") tm))
      ))

(provide 'my-timestamp)
