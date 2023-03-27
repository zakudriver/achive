;;; achive-utils.el --- Extend for achive  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 zakudriver

;; Author: zakudriver <zy.hua1122@gmail.com>
;; URL: https://github.com/zakudriver/achive
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

;; Some util function for achive.

;;; Install these required packages:

;; + cl-lib

;;; Code:

(require 'cl-lib)


(defun achive-make-percent (price yestclose open)
  "Get stocks percent by (PRICE - YESTCLOSE) / YESTCLOSE, Return \"+/- xx%\".
If OPEN is \"0.00\", percent just is 0.00%."
  (if (zerop open)
      "0.00%"
    (unless (floatp price)
      (setq price (float price)))
    (unless (floatp yestclose)
      (setq yestclose (float yestclose)))
    (let ((result (/ (- price yestclose)
                     (if (zerop yestclose)
                         1.0 yestclose))))
      (format "%s%0.2f%%" (if (> result 0) "+" "") (* result 100)))))


(defmacro achive-set-timeout (callback seconds)
  "Like `setTimeout' for javascript.
CALLBACK: callback function.
SECONDS: integer of seconds."
  `(let ((timer))
     (setq timer (run-with-timer ,seconds nil (lambda ()
                                                (cancel-timer timer)
                                                (funcall ,callback timer))))))


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


(defun achive-readcache (path)
  "Read cache file of stock codes.
PATH: path of file dir."
  (if (file-exists-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (read (current-buffer)))))


(defun achive-writecache (path codes)
  "Write stock codes to cache file.
PATH: path of file dir.
CODES: list of stock codes."
  (with-temp-file path
    (prin1 codes (current-buffer))))


(defun achive-remove-nth-element (list index)
  "Remove LIST element by INDEX."
  (if (< (length list) (1+ index))
      nil
    (if (zerop index) (cdr list)
      (let ((last (nthcdr (1- index) list)))
        (setcdr last (cddr last))
        list))))


(defun achive-make-name (list _fields)
  "Make stock name by decode `gb18030'.
LIST: list of a stock value.
FIELDS: list of field index."
  (decode-coding-string (nth 1 list) 'gb18030))


(defun achive-make-change-percent (list fields)
  "Call function `achive-make-percent' to make `change-percent'.
LIST: list of a stock value.
FIELDS: list of field index."
  (achive-make-percent (string-to-number (nth (cdr (assoc 'price fields)) list))
                       (string-to-number (nth (cdr (assoc 'yestclose fields)) list))
                       (string-to-number (nth (cdr (assoc 'open fields)) list))))


(defun achive-make-volume (list _fields)
  "Get volume of display, current volume / 100.
LIST: list of a stock value.
FIELDS: list of field index."
  (number-to-string (/ (string-to-number (nth 9 list)) 100)))


(defun achive-make-turn-volume (list _fields)
  "Get turn-volume of display, current turn-volume / 10000, unit W (10000).
LIST: list of a stock value.
FIELDS: list of field index."
  (format "%dW" (/ (string-to-number (nth 10 list)) 10000)))


(defmacro achive-number-sort (index)
  "Create value of number sorting by INDEX."
  `(lambda (a b)
     (let ((get-percent-number (lambda (arg)
                                 (string-to-number (aref (cadr arg) ,index)))))
       (> (funcall get-percent-number a) (funcall get-percent-number b)))))


(defun achive-valid-entry-p (entry)
  "Check ENTRY data of valid."
  (not (string= (aref (cadr entry) 1) "-")))


(defun achive-working-time-p (buffer-name)
  "Whether it is working time or not.
If at 9:00 - 11:30 or 13:00 - 15:00 and visual buffer named
BUFFER-NAME is existing,
return t. Otherwise, return nil."
  (if (get-buffer-window buffer-name)
      (or (and (not (achive-compare-time "9:00")) (achive-compare-time "11:30"))
          (and (not (achive-compare-time "13:00")) (achive-compare-time "15:00")))
    nil))


(defun achive-weekday-p ()
  "Whether it is weekend or not."
  (let ((week (format-time-string "%w")))
    (not (or (string= week "0") (string= week "6")))))


(defun achive-remove-face (faced)
  "Remove face for FACED to extract text."
  (let ((end (length faced)))
    (set-text-properties 0 end nil faced)
    faced))


(provide 'achive-utils)

;;; achive-utils.el ends here

