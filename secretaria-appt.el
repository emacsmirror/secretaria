;;; secretaria-appt.el --- Functions dealing with org-mode appointments

;; Copyright (C) 2016-2017 Jorge Javier Araya Navarro

;; Author: Jorge Javier Araya Navarro <jorge@esavara.cr>
;; Keywords: convenience, appt
;; Homepage: https://bitbucket.org/shackra/secretaria

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

;; secretaria-appt.el contains functions use to deal with org-mode appointments

;;; Prayer:

;; Domine Iesu Christe, Fili Dei, miserere mei, peccatoris
;; Κύριε Ἰησοῦ Χριστέ, Υἱὲ τοῦ Θεοῦ, ἐλέησόν με τὸν ἁμαρτωλόν.
;; אדון ישוע משיח, בנו של אלוהים, רחם עליי, החוטא.
;; Nkosi Jesu Kristu, iNdodana kaNkulunkulu, ngihawukele mina, isoni.
;; Señor Jesucristo, Hijo de Dios, ten misericordia de mí, pecador.
;; Herr Jesus Christus, Sohn Gottes, hab Erbarmen mit mir Sünder.
;; Господи, Иисусе Христе, Сыне Божий, помилуй мя грешного/грешную.
;; Sinjoro Jesuo Kristo, Difilo, kompatu min pekulon.
;; Tuhan Yesus Kristus, Putera Allah, kasihanilah aku, seorang pendosa.
;; Bwana Yesu Kristo, Mwana wa Mungu, unihurumie mimi mtenda dhambi.
;; Doamne Iisuse Hristoase, Fiul lui Dumnezeu, miluiește-mă pe mine, păcătosul.
;; 主耶穌基督，上帝之子，憐憫我罪人。

;;; Code:

(require 'f)
(require 'org)
(require 'org-agenda)
(require 'subr-x)
(require 'alert)
(eval-when-compile
  (require 'cl))

(defvar secretaria-uknown-time-reminder-timer nil
  "Timer for periodically remind the user about pending tasks.")

(defcustom secretaria-unknown-time-appt-remind-time 30
  "Minutes before firing a reminder about tasks for today with no specified time of the day."
  :type 'interger
  :group 'secretaria)

(defun secretaria--skip-entry-if-done ()
  "Skip `org-mode' entries if they are DONE."
  (org-agenda-skip-entry-if 'done))

(defun secretaria--conditional-severity ()
  "Return a severity level for Alert if Emacs is ran as a daemon."
  (if (daemonp)
      'high
    'normal))

(defun secretaria--trim-replace (string)
  "Trim and replace any %d notation of STRING."
  (string-trim
   (replace-regexp-in-string "%[0-9]?d" "[0-9 ]+" string)))

(defun secretaria--leaders-prepare (&optional today)
  "Return a regexp for due `org-agenda' leaders.

If TODAY is non-nil, return a regexp string that will
match tasks scheduled or with deadline for today"
  (let* ((leaders))
    (if today
        (progn
          (push (car org-agenda-scheduled-leaders) leaders)
          (push (car org-agenda-deadline-leaders) leaders))
      (progn
        (push (car (cdr org-agenda-scheduled-leaders)) leaders)
        (push (car (cdr (cdr org-agenda-deadline-leaders))) leaders)))
    (mapconcat 'identity (cl-map 'list #'secretaria--trim-replace leaders) "\\|")))

(defun secretaria-alert-due-appt ()
  "Tell the user about due TODOs tasks."
  (let (appts (secretaria-get-appt 'due))
    (when (> 0 (length appts))
      (alert (format "Due entries: %s" (length appts))
             :title "Attention, boss!"
             :severity 'high
             :mode 'org-mode))))

(defun secretaria-alert-unknown-time-appt ()
  "Tell the user about tasks scheduled for today.

Those tasks have no time of the day specified"
  (let (appts (secretaria-get-appt 'unknown))
    (dolist (entry appts)
      (alert "Task for today, time unspecified"
             :title (or entry "(no title)")
             :severity (secretaria--conditional-severity)
             :mode 'org-mode))))

(defun secretaria-alert-after-init ()
  "Entry point for all alerts about appointments."
  (secretaria-alert-due-appt)
  (secretaria-alert-unknown-time-appt))

(defun secretaria-org-file-agenda-p (filename-directory)
  "Return t if FILENAME-DIRECTORY is in `org-agenda-files'."
  (when (not (eq filename-directory nil))
    (if (listp org-agenda-files)
        (or (member filename-directory (org-agenda-files))
            (file-in-directory-p (file-name-nondirectory filename-directory) org-directory)))))

(defun secretaria-get-appt (kind)
  "Return a list of appointments.

KIND is either 'due or 'unknown.  'due is for due appointments,
'unknown is for today appointments with unspecified time of day"
  (let* ((files (org-agenda-files))
         (entries)
         (appts)
         (today (calendar-current-date))
         (regexp (secretaria--leaders-prepare (if (eq kind 'due) nil t)))
         (org-agenda-skip-function '(secretaria--skip-entry-if-done))
         (org-clock-current-task (or org-clock-current-task "")))
    (dolist (file files)
      ;; Took from https://github.com/kiwanami/emacs-calfw/issues/26#issuecomment-24881831
      (setf org-agenda-buffer
            (when (buffer-live-p org-agenda-buffer)
              org-agenda-buffer))
      (setf entries (org-agenda-get-day-entries file today :scheduled :deadline))
      (dolist (entry entries)
        (when (or (and (eq kind 'unknown)
                       (string-match-p regexp (get-text-property 0 'extra entry))
                       (string-empty-p (get-text-property 0 'time entry))
                       (not (string-equal org-clock-current-task (substring-no-properties (get-text-property 0 'txt entry)))))
                  (and (eq kind 'due)
                       (string-match-p regexp (get-text-property 0 'extra entry))))
          (push (substring-no-properties (get-text-property 0 'txt entry)) appts))))
    appts))

(defun secretaria-after-save-update-appt ()
  "Update appointments if the saved file is part of `org-agenda-files'."
  (interactive)
  (when (eq major-mode 'org-mode)
    (when (secretaria-org-file-agenda-p buffer-file-name)
      (org-agenda-to-appt t))))

(defun secretaria-unknown-time-appt-always-remind-me ()
  "Timer for reminders of today tasks which have an unknown time of the day.

If the variable `secretaria-unknown-time-appt-remind-time'
is 0, use the default of 30 minutes (to avoid accidents made by
the user)."
  (setf secretaria-uknown-time-reminder-timer
        (run-at-time (format "%s min" (or secretaria-unknown-time-appt-remind-time 30))
                     (* (or secretaria-unknown-time-appt-remind-time 30) 60) 'secretaria-alert-unknown-time-appt)))

(add-hook 'after-init-hook #'secretaria-alert-after-init)
(add-hook 'after-save-hook #'secretaria-after-save-update-appt)

(provide 'secretaria-appt)
;;; secretaria-appt.el ends here
