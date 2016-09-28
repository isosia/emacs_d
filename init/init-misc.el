;;; init-misc.el --- 

;;********** コーディング支援 **********;;
;; C-x 3 とかしたときも行の折り返しをする
(setq truncate-partial-width-windows nil)
(defun toggle-truncate-lines-partial ()
  "行の折り返しをトグル動作します"
  (interactive)
  (if truncate-partial-width-windows
      (setq truncate-partial-width-windows nil)
    (setq truncate-partial-width-windows t))
  (recenter))
(global-set-key "\C-c\C-k" 'toggle-truncate-lines-partial)
(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))
(global-set-key "\C-c\C-l" 'toggle-truncate-lines) ; 折り返し表示ON/OFF

;; メモを C-x M で出てくるように
(defun memo()
  (interactive)
  (find-file "/home/m_kanazawa/Documents/memo.rst"))
(define-key ctl-x-map "M" 'memo)

;; fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (if (fboundp 'ns-toggle-fullscreen)
      (ns-toggle-fullscreen)
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                       nil 'fullboth))))
(global-set-key "\C-cl" 'toggle-fullscreen)

;; 終了時に、プロセスがいるんですけどどうしますかと確認させない
;; http://d.hatena.ne.jp/kitokitoki/20101029
(defadvice save-buffers-kill-terminal
  (before my-save-buffers-kill-terminal activate)
  (when (process-list)
    (dolist (p (process-list))
      (set-process-query-on-exit-flag p nil))))

;; ansi-termの色を黒背景でも見やすくする
(setq ansi-term-color-vector
      [unspecified "black" "red1" "lime green" "yellow2"
                   "DeepSkyBlue3" "magenta2" "cyan2" "white"])

;; diredのソート種類を増やす
; http://d.hatena.ne.jp/mooz/20091207/p1
(defvar dired-various-sort-type
  '(("S" . "size")
    ("X" . "extension")
    ("v" . "version")
    ("t" . "date")
    (""  . "name")))

(defun dired-various-sort-change (sort-type-alist &optional prior-pair)
  (when (eq major-mode 'dired-mode)
    (let* (case-fold-search
           get-next
           (options
            (mapconcat 'car sort-type-alist ""))
           (opt-desc-pair
            (or prior-pair
                (catch 'found
                  (dolist (pair sort-type-alist)
                    (when get-next
                      (throw 'found pair))
                    (setq get-next (string-match (car pair) dired-actual-switches)))
                  (car sort-type-alist)))))
      (setq dired-actual-switches
            (concat "-l" (dired-replace-in-string (concat "[l" options "-]")
                                                  ""
                                                  dired-actual-switches)
                    (car opt-desc-pair)))
      (setq mode-name
            (concat "Dired by " (cdr opt-desc-pair)))
      (force-mode-line-update)
      (revert-buffer))))

(defun dired-various-sort-change-or-edit (&optional arg)
  "Hehe"
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-various-sort-change dired-various-sort-type)))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "s" 'dired-various-sort-change-or-edit))
             )

;; 諸々
(mouse-wheel-mode t)			; マウスホイール
(setq mouse-wheel-follow-mouse t)	; マウスホイール
(setq line-number-mode t)		; 行番号
(setq column-number-mode t)		; 列番号
(setq display-time-24hr-format t)       ; 24 時間表記
(setq display-time-day-and-date t)      ; 日付も
(setq display-time-string-forms
      '(month "/" day "(" dayname ") " 24-hours ":" minutes))
(display-time)				; 現在時刻
(set-scroll-bar-mode 'right)		; 右にスクロールバー表示
(fset 'yes-or-no-p 'y-or-n-p)		; yes or no -> y or n
(require 'server)
(unless (server-running-p)
  (server-start)
  )
(require 'generic-x)
(windmove-default-keybindings)          ; Shift + 矢印キー でウィンドウ間移動
(setq windmove-wrap-around t)           ; Shift + 矢印キー でウィンドウ間移動
(winner-mode)				; winner-mode 画面分割の状態を記憶
; ツールバーを消す
(if (fboundp 'ns-toggle-toolbar)
    (ns-toggle-toolbar)
  (tool-bar-mode -1))
(menu-bar-mode -1)			; ツールバーを消す
(setq case-fold-search t)   ; 検索や置換の時に大文字小文字を区別しない
(setq save-place t)	    ; 前回編集していた場所を記憶
(show-paren-mode t)	    ; 括弧の対応を光らせる
(setq show-paren-style 'mixed) ; ウィンドウ内に収まらないときだけ括弧内も光らせる。
(transient-mark-mode t)	    ; 選択範囲を強調表示
(put 'upcase-region 'disabled nil)	; C-c C-uを有効に
(recentf-mode t)			; 最近使ったファイル
(setq recentf-max-saved-items 10000)
(savehist-mode t)
(setq-default indent-tabs-mode nil)	; インデントに空白を使う
(setq use-file-dialog nil)              ; GTK+でファイル選択ダイアログボックスが出てこないようにする
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq history-length t);; ミニバッファ履歴リストの最大長：tなら無限
(put 'downcase-region 'disabled nil) ; リージョンに対する大文字小文字変換コマンドである upcase-regionとdowncase-regionは、 通常、使用禁止になっている

(setq-default indicate-empty-lines t) ; バッファの終端がフリンジでわかるようになる
(setq-default indicate-buffer-boundaries 'right) ; 右フリンジにバッファの終端に表示

(setq compilation-scroll-output t) ;; コンパイルバッファの追跡

(setq kill-whole-line t) ;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq require-final-newline t) ;;; 最終行に必ず一行挿入する
(setq next-line-add-newlines nil);;; バッファの最後でnewlineで新規行を追加するのを禁止する

;(pc-selection-mode)	 ; Shift + 矢印キー で範囲選択できるようにする
;(cond ((null window-system)
;       (xterm-mouse-mode))) ; -nwでもマウスが使えるモード
;(fringe-mode 0) ; 横のフリンジ表示しないモード


(setq-default tab-width 4)     ;; tab 幅を 4 に設定

(setq default-directory "~/") ;デフォルトディレクトリ

; 関数名表示を有効化
(which-function-mode 1)

; 起動時の画面を非表示にする
(setq inhibit-startup-message t)

; 句読点の変更
(setq its-hira-period "．")
(setq its-hira-comma "，")


(provide 'init-misc)
;;; init-misc.el ends here
