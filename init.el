;;; emacs -q -lした時に、user-emacs-directoryが変わるように
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; ロードパス追加設定
(add-to-list 'load-path (locate-user-emacs-file "elisp/el-get/el-get/"))

(setq el-get-dir "~/.emacs.d/elisp/el-get/")

;;; el-getが無かった場合は，クローンしてくる
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;;; ~/.emacs.d/init以下に設定ファイルを置く
(setq el-get-user-package-directory (locate-user-emacs-file "init"))

;;;
(require 'el-get)
(provide 'el-get-init)
(el-get 'sync)


