;;; aleek.el --- A-stocks real-time price.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 dingansich_kum0

;; Author: dingansich_kum0 <zy.hua1122@outlook.com>
;; URL: https://github.com/dingansichKum0/aleek
;; Version: 1.0
;; Package-Requires: ((emacs "25.2")
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

;;; Code:

;;;; Requirements

(require 'aleek-utils)
(require 'url)
(require 'org-table)


(defvar url-http-response-status)

;;;; Customization

(defgroup aleek nil
  "Settings for `aleek'."
  :prefix "aleek-"
  :group 'utils)


(defcustom aleek-api "http://hq.sinajs.cn"
  "Stocks Api."
  :group 'aleek
  :type 'string)


(defcustom aleek-storcks-list '("sh600036" "sz000625")
  "A-Stocks list."
  :group 'aleek
  :type 'list)


(defcustom aleek-buffer-name "*A Leek*"
  "Stocks buffer name."
  :group 'aleek
  :type 'string)


(defcustom aleek-language 'en
  "Current language."
  :group 'aleek
  :type '(choice
          (const :tag "English" 'en)
          (const :tag "中文" 'zh)))

(defcustom aleek-index-list '("sh000001" "sz399001" "sz399006")
  "List of composite index."
  :group 'aleek
  :type 'list)

;;;; constants

(defconst aleek-stocks-header '("|-\n| code | name | price | percent | high | low | volume | turn-volume | open | yestclose |\n|-\n" "|-\n| 股票代码 | 名称 | 当前价 | 涨跌幅 | 最高价 | 最低价 | 成交量 | 成交额 | 开盘价 | 昨日收盘价 |\n|-\n")
  "Stocks list headern.")


(defconst aleek-index-title '("** Composite Index" "** 大盘指数")
  "Stocks index title.")


(defconst aleek-stocks-title '("** Individual Stocks" "** 个股")
  "Stocks list title.")


(defconst aleek-visual-table-row-format
  "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |\n|-\n"
  "Stock-Tracker result item format.")


(defun aleek-make-request-url (api parameter)
  "Make sina request url.
API: shares api.
PARAMETER: request url parameter."
  (format "%s/list=%s" api (string-join parameter ",")))


(defun aleek-request (url callback)
  "Request function.
URL: string of url.
CALLBACK: function"
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json"))))
    (url-retrieve url (lambda (_status) (funcall callback)) nil 'silent)))


(defun aleek-parse-response ()
  "Parse sina http response result by body."
  (set-buffer-multibyte t)
  (goto-char (point-min))
  (if (/= 200 url-http-response-status)
      (error "Problem connecting to the server"))
  (let ((resp-gbcode (with-current-buffer (current-buffer)
                       (buffer-substring-no-properties (search-forward "\n\n") (point-max)))))
    (decode-coding-string resp-gbcode 'gb18030 t)))


(defun aleek-format-content (stocks resp-str)
  "Format response string to buffer string.
RESP-STR: string of response body.
STOCKS: stocks list of request parameters.
Return index and stocks data."
  (let ((str-list (cl-loop with i = 0
                           for it in stocks
                           if (string-match (format "%s=\"\\([^\"]+\\)\"" it) resp-str)
                           collect (format "%s,%s" (nth i stocks) (match-string 1 resp-str))
                           else
                           collect (nth i stocks) end
                           do (cl-incf i))))
    (cl-loop with r = ""
             for it in str-list
             do (setq r (concat r (apply 'format aleek-visual-table-row-format (aleek-format-row it))))
             finally return r)))


(defun aleek-format-row (row-str)
  "Format row content.
ROW-STR: string of row."
  (let ((field-index-list
         '((code . 0) (name . 1) (price . 4) (percent . aleek-call-compute-percent)
           (high . 5) (low . 6) (volume . 9) (turn-volume . 10) (open . 2) (yestclose . 3)))
        (value-list (split-string row-str ",")))
    (if (= 1 (length value-list))
        (append value-list '("-" "-" "-" "-" "-" "-" "-" "-" "-"))
      (cl-loop for (_k . v) in field-index-list
               collect (if (integerp v)
                           (nth v value-list)
                         (funcall v value-list field-index-list))))))


(defun aleek-call-compute-percent (list fields)
  "Call function `aleek-compute-percent'.
LIST: list of a stock value.
FIELDS: list of field index."
  (aleek-compute-percent (string-to-number (nth (cdr (assoc 'price fields)) list))
                         (string-to-number (nth (cdr (assoc 'yestclose fields)) list))))

;; (aleek-format-row "sh600036,招商银行,51.700,51.600,51.790,52.330,50.410,51.790,51.800,49684591,2564547290.000,12300,51.790,12900,51.780,1900,51.770,1900,51.760,2100,51.750,3700,51.800,45339,51.810,23000,51.820,12600,51.830,8400,51.840,2021-09-03,13:59:53,00,")


;; (aleek-request (aleek-make-request-url aleek-api aleek-storcks-list) 'aleek-parse-response)


(defun aleek-visual-render (indexs stocks)
  "Render stocks list.
Insert string of TIME, INDEXS and STOCKS."
  (with-current-buffer (get-buffer-create aleek-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (aleek-visual-mode)
      (insert "** " (aleek-format-time-local aleek-language) "\n\n")

      (when indexs
        (insert (aleek-text-local aleek-index-title aleek-language) "\n")
        (insert (aleek-text-local aleek-stocks-header aleek-language))
        (insert indexs))
      
      (when (stringp stocks)
        (insert (aleek-text-local aleek-stocks-title aleek-language) "\n")
        (insert (aleek-text-local aleek-stocks-header aleek-language))
        (insert stocks))

      
      
      (org-table-map-tables 'org-table-align t))
    (unless (get-buffer-window (current-buffer))
      (switch-to-buffer-other-window aleek-buffer-name))))

;;;;; Keymaps

(defvar aleek-visual-mode-map (make-sparse-keymap "aleek visual mode")
  "Aleek-visual-mode keymap.")


(define-key aleek-visual-mode-map "q" 'quit-window)

;;;;; interactive

;;;###autoload
(defun aleek ()
  "Launch aleek."
  (interactive)
  (aleek-visual-render ))


(define-derived-mode aleek-visual-mode org-mode aleek-buffer-name
  "Major mode for aleek stocks view."
  :group 'aleek
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t
        show-trailing-whitespace nil)
  (setq-local line-move-visual t)
  (setq-local view-read-only nil)
  (run-mode-hooks))


(provide 'aleek)

;;; aleek.el ends here


(let ((str "var hq_str_sh600036=\"招商银行,51.700,51.600,51.760,52.330,50.410,51.750,51.770,47270791,2439328768.000,6800,51.750,17800,51.740,11000,51.730,1800,51.720,3700,51.710,11600,51.770,11700,51.780,17700,51.790,39000,51.800,900,51.810,2021-09-03,13:45:14,00,\";")
      (str1 "var hq_str_sz00062=\"\";")
      (str2 "var hq_str_sh600036=\"招商银行,51.700,51.600,51.790,52.330,50.410,51.790,51.800,49684591,2564547290.000,12300,51.790,12900,51.780,1900,51.770,1900,51.760,2100,51.750,3700,51.800,45339,51.810,23000,51.820,12600,51.830,8400,51.840,2021-09-03,13:59:53,00,\";
var hq_str_sz00062=\"\";
var hq_str_sh000001=\"上证指数,3602.7421,3597.0426,3587.3550,3613.9480,3575.7861,0,0,511535435,618974661263,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2021-09-03,13:59:45,00,\";")
      (list '("sh600036" "sz00062" "sh000001")))

  

  (aleek-visual-render (aleek-format-content list str2))
  ;; (aleek-format-content list str2)
  )


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
