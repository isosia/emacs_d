;;; init-coloring.el --- 

;;********** 色づけ **********;;
;; 色付け
(global-font-lock-mode t)
(transient-mark-mode 1)			; 選択範囲に色を付ける

(require 'ansi-color)
(add-hook 'compilation-filter-hook
          '(lambda ()
             (ansi-color-apply-on-region (point-min) (point-max))))

;; color-theme
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (if (window-system)
    ;(color-theme-robin-hood)
    (color-theme-clarity)
    ;(color-theme-greiner)
    (color-theme-emacs-nw)
    ))
(setq frame-background-mode 'dark)

;; 半角スペース＋タブ, 全角スペース、行末のスペース＆タブを色づけ
(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray26"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
(font-lock-add-keywords
 major-mode
 '(					;(" \t" 0 my-face-b-2 append)
   ("　" 0 my-face-b-1 append)
					;("[ \t]+$" 0 my-face-u-1 append)
   )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; powerline
(require 'powerline)
(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(custom-set-faces
 '(mode-line ((t (:foreground "#030303" :background "#dddddd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
;; http://hico-horiuchi.com/wiki/doku.php?id=emacs:powerline
;; バッファ情報の書式
(defpowerline buffer-id (propertize (car (propertized-buffer-identification "%b"))
                                    'face (powerline-make-face color1)))
(defpowerline row     "%l")    ; 行番号の書式
(defpowerline column  "%c")    ; 列番号の書式
(defpowerline percent "%p")    ; カーソル位置の割合
(defpowerline time    "%M")    ; 時計の書式
;; 右部分の位置合わせ(右端から何文字分を左に寄せるか、デフォルト+15文字)
(defun powerline-make-fill (color)
  (let ((plface (powerline-make-face color)))
    (if (eq 'right (get-scroll-bar-mode))
        (propertize " " 'display '((space :align-to (- right-fringe 36))) 'face plface)
      (propertize " " 'display '((space :align-to (- right-fringe 39))) 'face plface))))
;; Powerlineの書式
(setq-default mode-line-format (list
                                '("-" mode-line-mule-info mode-line-modified)
                                '(:eval (concat
                                         (powerline-buffer-id   'left   nil powerline-color1)
                                         (powerline-major-mode  'left       powerline-color1)
                                         (powerline-minor-modes 'left       powerline-color1)
                                         (powerline-narrow      'left       powerline-color1 powerline-color2)
                                         (powerline-vc          'center                      powerline-color2)
                                         (powerline-make-fill                                powerline-color2)
                                         (powerline-row         'right      powerline-color1 powerline-color2)
                                         (powerline-make-text   ": "        powerline-color1)
                                         (powerline-column      'right      powerline-color1)
                                         (powerline-percent     'right      powerline-color1)
                                         (powerline-time        'right  nil powerline-color1)
                                         (powerline-make-text   "  "    nil )))))
;; 時計のフォーマット
(setq display-time-string-forms '((format
                                   "%s/%s(%s) %s:%s" month day dayname 24-hours minutes)))
(display-time-mode t)    ; 時計を表示

(provide 'init-coloring)
;;; init-coloring.el ends here
