;;; secretaria-appt.el --- Functions dealing with org-mode appointments

;; Copyright (C) 2016 Jorge Javier Araya Navarro

;; Author: Jorge Javier Araya Navarro <elcorreo@deshackra.com>
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

(defcustom secretaria/today-unknown-time-appt-remind-every 30 "Minutes before firing a reminder about tasks for today with no specified time of the day" :type 'interger)

(defun secretaria/leaders--prepare (fortodaytasks)
  "Return a regexp for due org-agenda leaders.
If `FORTODAYTASKS' is non-nil, return a regexp string that will
match tasks scheduled or with a deadline for today"
  (let* ((leaders (append org-agenda-deadline-leaders org-agenda-scheduled-leaders))
         (regexpleaders '()))
    (dolist (leader leaders)
      (setf leader (string-trim leader))
      (if fortodaytasks
          ;; FIXME: This `when' `when' thing feels weird
          (when (not (string-match-p "%[0-9]?d" leader))
            (push leader regexpleaders))
        (when (string-match-p "%[0-9]?d" leader)
          (push (replace-regexp-in-string "%[0-9]?d" "[0-9]+" leader) regexpleaders))))
    (mapconcat 'identity regexpleaders "\\|")))

(defun secretaria/conditional--severity ()
  "Depending if Emacs is ran as daemon or not, return a severity level for Alert"
  (if (daemonp)
      'high
    'normal))

(defun secretaria/org-file-agenda-p (filename-directory)
  "Return t if FILENAME-DIRECTORY is in `org-agenda-files'"
  (when (not (eq filename-directory nil))
    (if (listp org-agenda-files)
        (or (member filename-directory (org-agenda-files))
           (file-in-directory-p (file-name-nondirectory filename-directory) org-directory)))))

(defun secretaria/due-appt ()
  "Tell the user about due TODOs tasks"
  (let* ((files (org-agenda-files))
         (due-todos 0)
         (today (calendar-current-date))
         (due-regexp (secretaria/leaders--prepare nil)))
    (dolist (file files)
      (let* ((entries (org-agenda-get-day-entries file today :scheduled :deadline)))
        (dolist (entry entries)
          ;; Get how many times the task was re-scheduled, and count it
          (when (string-match-p due-regexp (get-text-property 0 'extra entry))
            (setf due-todos (1+ due-todos))))))
    (when (> due-todos 0)
      (alert (format "You have <b>%d due task%s</b>! please check org-agenda." due-todos (if (>= due-todos 2) "s" ""))
             :title "I need your attention urgently, boss!"
             :severity 'high
             :style secretaria/style-best-available
             :mode 'org-mode))))

(defun secretaria/today-unknown-time-appt ()
  "Tell the user about tasks scheduled for today.
Those scheduled tasks for today have no time of the day specified"
  (let* ((files (org-agenda-files))
         (today (calendar-current-date))
         (today-regexp (secretaria/leaders--prepare t)))
    (dolist (file files)
      (let* ((entries (org-agenda-get-day-entries file today :scheduled :deadline)))
        (dolist (entry entries)
          (if (and (string-match-p today-regexp (get-text-property 0 'extra entry))
                 (string-empty-p (get-text-property 0 'time entry)))
              (alert (format "%s" (get-text-property 0 'txt entry))
                     :title "Task for today with an unknown time of day"
                     :severity (secretaria/conditional--severity)
                     :style secretaria/style-best-available
                     :mode 'org-mode)))))))

(defun secretaria/update-appt ()
  "Update appointments if the saved file is part of the agendas files"
  (interactive)
  (when (eq major-mode 'org-mode)
    (when (secretaria/org-file-agenda-p buffer-file-name)
      (org-agenda-to-appt t))))

(defun secretaria/today-unknown-time-appt-always-remind-me ()
  "Prepare a timer for firing reminders of tasks for today, with an unknown time of the day.
If the variable `secretaria/today-unknown-time-appt-remind-every' is 0, use the default
of 30 minutes (to avoid accidents made by the user)."
  (setf secretaria/today-uknown-time-reminder-timer
        (run-at-time (format "%s min" (or secretaria/today-unknown-time-appt-remind-every 30))
                     (* (or secretaria/today-unknown-time-appt-remind-every 30) 60) 'secretaria/today-unknown-time-appt)))

(add-hook 'after-init-hook #'secretaria/today-unknown-time-appt)
(add-hook 'after-save-hook #'secretaria/update-appt)
(add-hook 'after-init-hook #'secretaria/due-appt)

(provide 'secretaria-appt)
