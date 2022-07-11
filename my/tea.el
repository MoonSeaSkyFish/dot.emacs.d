;#+date: 2022-07-07T22:07:36Z
;#+last: 2022-07-07T22:14:41Z

;; package macro



(defmacro tea (_pkg &rest _args)

)


(defmacro no-tea (_pkg))
(defalias 'notea 'no-tea)


(provide 'tea)
