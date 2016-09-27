;;; emacs -q -lした時に、user-emacs-directoryが変わるように
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;; ロードパス追加設定
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get/"))

(setq el-get-dir "~/.emacs.d/el-get/")

(setq el-get-generate-autoloads t)
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
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(defvar my/el-get-packages
  '(howm))
(el-get 'sync my/el-get-packages)



(require 'cl-lib)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar my/packages
  '(
    yaml-mode yasnippet
    ))
(let ((not-installed
       (cl-loop for x in my/packages
                when (not (package-installed-p x))
                collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))
