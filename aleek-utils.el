;;; aleek-utils.el --- Extend for aleek  -*- lexical-binding: t; -*-

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

;; Some util function for aleek.

;;; Install these required packages:

;; + cl-lib

;;; Code:

(require 'cl-lib)


(defun aleek-matchs-to-list (reg string)
  "Get a list of all REG matche string in a STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match reg string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))


(defun aleek-text-local (texts lang)
  "Choice text by language.
TEXTS: list of string.
LANG: symbol of language type."
  ;; (let* ((langs '(en zh))
  ;;        (index (cl-position lang langs :test 'equal)))
  ;;   (nth index texts))
  (if (eq lang 'en)
      (car texts)
    (car (cdr texts))))


(defun aleek-format-time-local (lang)
  "Generate time of localized.
LANG: symbol of language type."
  (if (eq lang 'en)
      (format-time-string "%H:%M:%S %A %d-%m-%G")
    (let ((weeks '("日" "一" "二" "三" "四" "五" "六"))
          (week-index (string-to-number (format-time-string "%w"))))
      (format "%s 星期%s" (format-time-string "%G-%m-%d %H:%M:%S") (nth week-index weeks)))))


(defun aleek-compute-percent (price yestclose)
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


(provide 'aleek-utils)

;;; aleek-utils.el ends here

