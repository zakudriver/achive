;;; achive.el --- A-stocks real-time data  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 dingansich_kum0

;; Author: dingansich_kum0 <zy.hua1122@outlook.com>
;; URL: https://github.com/dingansichKum0/achive
;; Version: 1.0
;; Package-Requires: ((emacs "25.2"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Achive is a plug-in based on api of Sina that creates a dashboard displaying real-time data of a-share indexs and stocks.
;; Thanks for the super-fast Sina api, and achive performs so well to update data automatically.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'url)
(require 'org-table)
(require 'achive-utils)


(defvar url-http-response-status)

;;;; Customization

(defgroup achive nil
  "Settings for `achive'."
  :prefix "achive-"
  :group 'utils)


(defcustom achive-stock-list '("sh600036" "sz000625")
  "List of stocks."
  :group 'achive
  :type 'list)


(defcustom achive-buffer-name "*A Chive*"
  "Stocks buffer name."
  :group 'achive
  :type 'string)


(defcustom achive-language 'en
  "Displayed language."
  :group 'achive
  :type '(choice
          (const :tag "English" 'en)
          (const :tag "中文" 'zh)))

(defcustom achive-index-list '("sh000001" "sz399001" "sz399006")
  "List of composite index."
  :group 'achive
  :type 'list)


(defcustom achive-auto-refresh t
  "Whether to refresh automatically."
  :group 'achive
  :type 'boolean)


(defcustom achive-refresh-seconds 5
  "Seconds of automatic refresh time."
  :group 'achive
  :type 'integer)


(defcustom achive-display-indexs t
  "Whether to show indexs."
  :group 'achive
  :type 'boolean)

;;;; constants

(defconst achive-api "http://hq.sinajs.cn"
  "Stocks Api.")


(defconst achive-stocks-header '("|-\n| code | name | price | percent | high | low | volume | turn-volume | open | yestclose |\n|-\n" "|-\n| 股票代码 | 名称 | 当前价 | 涨跌幅 | 最高价 | 最低价 | 成交量 | 成交额 | 开盘价 | 昨日收盘价 |\n|-\n")
  "Stocks list headern.")


(defconst achive-index-title '("** Composite Index" "** 大盘指数")
  "Stocks index title.")


(defconst achive-stocks-title '("** Individual Stocks" "** 个股")
  "Stocks list title.")


(defconst achive-visual-table-row-format
  "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n|-\n"
  "Stock-Tracker result item format.")

;;;;; variables

(defvar achive-prev-point nil
  "Point of before render.")

;;;;; functions

(defun achive-make-request-url (api parameter)
  "Make sina request url.
API: shares api.
PARAMETER: request url parameter."
  (format "%s/list=%s" api (string-join parameter ",")))


(defun achive-request (url callback)
  "Handle request by URL.
CALLBACK: function of after response."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/javascript;charset=UTF-8"))))
    (url-retrieve url (lambda (_status)
                        (let ((inhibit-message t))
                          (message "achive: %s at %s" "The request is successful." (format-time-string "%T")))
                        (funcall callback)) nil 'silent)))


(defun achive-parse-response ()
  "Parse sina http response result by body."
  (if (/= 200 url-http-response-status)
      (error "Internal Server Error."))
  (let ((resp-gbcode (with-current-buffer (current-buffer)
                       (buffer-substring-no-properties (search-forward "\n\n") (point-max)))))
    (decode-coding-string resp-gbcode 'gb18030)))


(defun achive-format-content (codes resp-str)
  "Format response string to buffer string.
RESP-STR: string of response body.
CODES: stocks list of request parameters.
Return index and stocks data."
  (let ((str-list (cl-loop with i = 0
                           for it in codes
                           if (string-match (format "%s=\"\\([^\"]+\\)\"" it) resp-str)
                           collect (format "%s,%s" (nth i codes) (match-string 1 resp-str))
                           else
                           collect (nth i codes) end
                           do (cl-incf i))))
    (cl-loop with r = ""
             for it in str-list
             do (setq r (concat r (apply 'format achive-visual-table-row-format (achive-format-row it))))
             finally return r)))


(defun achive-format-row (row-str)
  "Format row content.
ROW-STR: string of row."
  (let ((field-index-list
         '((code . 0) (name . 1) (price . 4) (percent . achive-call-make-percent)
           (high . 5) (low . 6) (volume . achive-make-volume) (turn-volume . achive-make-turn-volume) (open . 2) (yestclose . 3)))
        (value-list (split-string row-str ",")))
    (if (= 1 (length value-list))
        (append value-list '("-" "-" "-" "-" "-" "-" "-" "-" "-"))
      (cl-loop for (_k . v) in field-index-list
               collect (if (functionp v)
                           (funcall v value-list field-index-list)
                         (nth v value-list))))))


(defun achive-call-make-percent (list fields)
  "Call function `achive-make-percent'.
LIST: list of a stock value.
FIELDS: list of field index."
  (achive-make-percent (string-to-number (nth (cdr (assoc 'price fields)) list))
                       (string-to-number (nth (cdr (assoc 'yestclose fields)) list))))


(defun achive-make-volume (list _fields)
  "Get volume of display, current volume / 100.
LIST: list of a stock value.
FIELDS: list of field index."
  (/ (string-to-number (nth 9 list)) 100))


(defun achive-make-turn-volume (list _fields)
  "Get turn-volume of display, current turn-volume / 10000, unit W (10000).
LIST: list of a stock value.
FIELDS: list of field index."
  (format "%dW" (/ (string-to-number (nth 10 list)) 10000)))


(defun achive-handle-request (codes &optional callback)
  "Handle request by stock code list.
CODES: list of stock code.
CALLBACK: after the rendering."
  (achive-request (achive-make-request-url achive-api codes)
                  (lambda ()
                    (let ((resp-str (achive-parse-response))
                          indexs stocks)
                      (if (listp achive-index-list)
                          (setq indexs (achive-format-content achive-index-list resp-str)))
                      (if (listp achive-stock-list)
                          (setq stocks (achive-format-content achive-stock-list resp-str)))
                      (achive-visual-render indexs stocks)
                      (if (functionp callback)
                          (funcall callback))))))


(defun achive-should-refresh ()
  "Current should be refresh.
If at 9:00 - 11:30 or 13:00 - 15:00 on weekdays and visual buffer is existing,
return t. Otherwise, return nil."
  (let ((week (format-time-string "%w"))
        should)
    (if (get-buffer-window achive-buffer-name)
        (unless (or (string= week "0") (string= week "6"))
          (setq should
                (or (and (not (achive-compare-time "9:00")) (achive-compare-time "11:30"))
                    (and (not (achive-compare-time "13:00")) (achive-compare-time "15:00"))))))
    should))


(defun achive-switch-visual ()
  "Switch to visual buffer."
  (unless (get-buffer achive-buffer-name)
    (with-current-buffer (get-buffer-create achive-buffer-name)
      (let ((inhibit-read-only t))
        (achive-visual-mode)
        
        (insert "** " (achive-format-time-local achive-language) "\n\n"))))
  (let ((window (get-buffer-window achive-buffer-name)))
    (if window
        (delete-window window)
      (switch-to-buffer-other-window achive-buffer-name))))


(defun achive-visual-render (indexs stocks)
  "Render stocks list.
Insert string of TIME, INDEXS and STOCKS."
  (with-current-buffer (get-buffer achive-buffer-name)
    (let ((inhibit-read-only t))
      (setq achive-prev-point (point))
      (erase-buffer)
      
      ;; (achive-visual-mode)
      (insert "** " (achive-format-time-local achive-language) "\n\n")

      (when (and achive-display-indexs (stringp indexs))
        (insert (achive-text-local achive-index-title achive-language) "\n")
        (insert (achive-text-local achive-stocks-header achive-language))
        (insert indexs))
      
      (when (stringp stocks)
        (insert "\n" (achive-text-local achive-stocks-title achive-language) "\n")
        (insert (achive-text-local achive-stocks-header achive-language))
        (insert stocks))

      (org-table-map-tables 'org-table-align t)
      (goto-char achive-prev-point))))


(defun achive-handle-auto-refresh (codes)
  "Automatic refresh.
CODES: list of stock code."
  (if (achive-should-refresh)
      (achive-set-timeout (lambda ()
                            (achive-handle-request codes (lambda ()
                                                           (achive-handle-auto-refresh codes))))
                          achive-refresh-seconds)))

;;;;; interactive

;;;###autoload
(defun achive ()
  "Launch achive and switch to visual buffer."
  (interactive)
  (if (achive-switch-visual)
      (let ((codes (append achive-index-list achive-stock-list)))
        (achive-handle-request codes)
        (if achive-auto-refresh
            (achive-handle-auto-refresh codes)))))


;;;###autoload
(defun achive-refresh ()
  "Manual refresh and render."
  (interactive)
  (if (get-buffer-window achive-buffer-name)
      (let ((codes (append achive-index-list achive-stock-list)))
        (achive-handle-request codes)
        (message "Achive has been refreshed."))))


;;;###autoload
(defun achive-exit ()
  "Exit achive."
  (interactive)
  (when (get-buffer achive-buffer-name)
    (quit-window t (get-buffer-window achive-buffer-name))
    (message "Achive has been killed.")))

;;;;; mode

(define-derived-mode achive-visual-mode org-mode achive-buffer-name
  "Major mode for achive stocks view."
  :group 'achive
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t
        show-trailing-whitespace nil
        buffer-file-coding-system 'gb18030
        line-spacing 0.1)
  (setq-local line-move-visual t
              view-read-only nil)
  
  (defface achive-buffer-local-face
    '((t :height 115))
    "achive-buffer-local face")
  (buffer-face-set 'achive-buffer-local-face)

  (local-set-key "q" 'achive-exit)
  (local-set-key "p" 'previous-line)
  (local-set-key "n" 'next-line)
  (local-set-key "g" 'achive-refresh)
  (run-mode-hooks))


(provide 'achive)

;;; achive.el ends here

;; (buffer-substring-no-properties (line-beginning-position) (line-end-position))

;; 0：”大秦铁路”，股票名字；
;; 1：”27.55″，今日开盘价；
;; 2：”27.25″，昨日收盘价；
;; 3：”26.91″，当前价格；
;; 4：”27.55″，今日最高价；
;; 5：”26.20″，今日最低价；
;; 6：”26.91″，竞买价，即“买一”报价；
;; 7：”26.92″，竞卖价，即“卖一”报价；
;; 8：”22114263″，成交的股票数，由于股票交易以一百股为基本单位，所以在使用时，通常把该值除以一百；
;; 9：”589824680″，成交金额，单位为“元”，为了一目了然，通常以“万元”为成交金额的单位，所以通常把该值除以一万；
;; 10：”4695″，“买一”申请4695股，即47手；
;; 11：”26.91″，“买一”报价；
;; 12：”57590″，“买二”
;; 13：”26.90″，“买二”
;; 14：”14700″，“买三”
;; 15：”26.89″，“买三”
;; 16：”14300″，“买四”
;; 17：”26.88″，“买四”
;; 18：”15100″，“买五”
;; 19：”26.87″，“买五”
;; 20：”3100″，“卖一”申报3100股，即31手；
;; 21：”26.92″，“卖一”报价
;; (22, 23), (24, 25), (26,27), (28, 29)分别为“卖二”至“卖四的情况”
;; 30：”2008-01-11″，日期；
;; 31：”15:05:32″，时间；
