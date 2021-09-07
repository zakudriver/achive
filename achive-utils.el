;;; achive-utils.el --- Extend for achive  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 dingansich_kum0

;; Author: dingansich_kum0 <zy.hua1122@outlook.com>
;; URL: https://github.com/dingansichKum0/achive
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

;; Some util function for achive.

;;; Install these required packages:

;; + cl-lib

;;; Code:

(require 'cl-lib)


(defun achive-matchs-to-list (reg string)
  "Get a list of all REG matche string in a STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match reg string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))


(defun achive-text-local (texts lang)
  "Choice text by language.
TEXTS: list of string.
LANG: symbol of language type."
  ;; (let* ((langs '(en zh))
  ;;        (index (cl-position lang langs :test 'equal)))
  ;;   (nth index texts))
  (if (eq lang 'en)
      (car texts)
    (car (cdr texts))))


(defun achive-format-time-local (lang)
  "Generate time of localized.
LANG: symbol of language type."
  (if (eq lang 'en)
      (format-time-string "%H:%M:%S %A %d-%m-%G")
    (let ((weeks '("日" "一" "二" "三" "四" "五" "六"))
          (week-index (string-to-number (format-time-string "%w"))))
      (format "%s 星期%s" (format-time-string "%G-%m-%d %H:%M:%S") (nth week-index weeks)))))


(defun achive-compute-percent (price yestclose)
  "Get stocks percent by (PRICE - YESTCLOSE) / yestclose.
Return '+-xx%'"
  (unless (floatp price)
    (setq price (float price)))
  (unless (floatp yestclose)
    (setq yestclose (float yestclose)))
  (if (= yestclose 0.0)
      (setq yestclose 1.0))
  (let ((result (/ (- price yestclose) yestclose)))
    (format "%0.2f%%" (* result 100))))


(defmacro achive-set-timeout (callback seconds)
  "Like `setTimeout' for javascript.
CALLBACK: callback function.
SECONDS: integer of seconds."
  `(let ((timer))
     (setq timer (run-with-timer ,seconds nil (lambda ()
                                                     (cancel-timer timer)
                                                     (funcall ,callback))))))


(defun achive-time-list-index (word)
  "Get index of time list by WORD."
  (let ((words '("seconds" "minutes" "hour" "day" "month" "year" "dow" "dst" "zone")))
    (cl-position word words :test 'equal)))


(defun achive-decoded-time (time word)
  "Like decoded-time-xxx(Emacs '27.1').
Get TIME object item by WORD."
  (nth (achive-time-list-index word) time))


(defun achive-time-number (str)
  "STR of '12:00' to integer of 1200."
  (if (stringp str)
      (string-to-number (replace-regexp-in-string (regexp-quote ":") "" str))
    0))


(defun achive-hhmm-to-time (hhmm &optional func)
  "Convert HHMM to time.
Callback FUNC is handle to time list."
  (if (stringp hhmm)
      (setq hhmm (achive-time-number hhmm)))
  (let* ((now (decode-time))
         (time-code (list 0 (% hhmm 100) (/ hhmm 100)
                          (achive-decoded-time now "day")
				                  (achive-decoded-time now "month")
                          (achive-decoded-time now "year")
                          (achive-decoded-time now "zone"))))
    (if (functionp func)
        (setq time-code (funcall func time-code)))
    (apply #'encode-time time-code)))


(defun achive-compare-time (hhmm)
  "Compare now and HHMM.
If now less than time return t."
  (let ((now (current-time))
        (time (achive-hhmm-to-time hhmm)))
    (time-less-p now time)))


(provide 'achive-utils)

;;; achive-utils.el ends here

