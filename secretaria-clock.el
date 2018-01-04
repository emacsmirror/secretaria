;;; secretaria-clock.el --- Org-mode clock related enhance

;; Copyright (C) 2016-2018 Jorge Araya Navarro

;; Author: Jorge Araya Navarro <jorge@esavara.cr>
;; Keywords: org, convenience
;; Homepage: https://bitbucket.org/shackra/secretaria.el

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

;; secretaria-clock.el contains functions use to enhance some things related to
;; org-mode clocks

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

(require 'org)
(require 'alert)
(require 's)
(require 'org-clock)
(require 'subr-x)

(defvar secretaria-clocked-in-reminder-timer nil
  "A timer set when the user clocks in a task.")

(defcustom secretaria-clocked-in-reminder-every-minutes 10
  "Minutes before firing a reminder of the task clocked in."
  :type 'integer
  :group 'secretaria)

(defvar secretaria--org-url-regexp (rx "[[" (group (one-or-more (not (any "]")))) "][" (group (one-or-more (not (any "]")))) "]]")
  "Search for links in `org-mode' markup.")

(defcustom secretaria-notification-handler-overwrite t
  "Tells Secretaria we want to use her notification function with `org-show-notification-handler'.  WARNING: Change this if you know what you are doing!."
  :type 'bool
  :group 'secretaria)

(defcustom secretaria-notification-to-html nil
  "Convert org markup to HTML, otherwise to plain text if nil."
  :type 'bool
  :group 'secretaria)

(defcustom secretaria-clocked-task-save-file (locate-user-emacs-file "secretaria-clocked-task")
  "File which keeps the name of the current clocked in task."
  :type 'file
  :group 'secretaria)

(defun secretaria--org-to-html (message)
  "Convert a MESSAGE in org markup to HTML."
  (unless message
    "")
  (if secretaria-notification-to-html
      (replace-regexp-in-string secretaria--org-url-regexp "<a href=\"\\1\">\\2</a>" message)
    (replace-regexp-in-string secretaria--org-url-regexp "\\2" message)))

(defun secretaria-task-clocked-time ()
  "Return a string with the clocked time and effort, if any."
  (interactive)
  (let* ((clocked-time (org-clock-get-clocked-time))
         (h (floor clocked-time 60))
         (m (- clocked-time (* 60 h)))
         (work-done-str (org-minutes-to-clocksum-string m)))
    (if org-clock-effort
        (let* ((effort-in-minutes
                (org-duration-string-to-minutes org-clock-effort))
               (effort-h (floor effort-in-minutes 60))
               (effort-m (- effort-in-minutes (* effort-h 60)))
               (effort-str (org-minutes-to-clocksum-string effort-m)))
          (format "%s/%s" work-done-str effort-str))
      (format "%s" work-done-str))))

(defun secretaria-notification-handler (notification)
  "Handle `org-mode' notifications.

`NOTIFICATION' is, well, the notification from `org-mode'"
  (if (not (s-contains? "should be finished by now" notification))
      (alert notification :title "Secretaria: message from org-mode" :mode 'org-mode)
    (alert (secretaria--org-to-html org-clock-current-task)
           :title (format "Task's estimate effort has been reach! (%s)" (secretaria-task-clocked-time))
           :severity 'high
           :mode 'org-mode)))

(defun secretaria-remind-task-clocked-in ()
  "Fires an alert for the user reminding him which task he is working on."
  (when org-clock-current-task
    (if (not org-clock-task-overrun)
        (alert (secretaria--org-to-html org-clock-current-task)
               :title "Currently clocked"
               :severity 'trivial
               :mode 'org-mode)
      (alert (secretaria--org-to-html org-clock-current-task)
             :title (format "Task's estimated effort exceeded! (%s)" (secretaria-task-clocked-time))
             :severity 'urgent
             :mode 'org-mode))))

(defun secretaria-task-clocked-in ()
  "Start a timer when a task is clocked-in."
  (secretaria-task-save-clocked-task)
  (setf secretaria-clocked-in-reminder-timer (run-at-time (format "%s min" (or secretaria-clocked-in-reminder-every-minutes 10)) (* (or secretaria-clocked-in-reminder-every-minutes 10) 60) 'secretaria-remind-task-clocked-in))
  (alert (secretaria--org-to-html org-clock-current-task)
         :title (format "Task clocked in! (%s)" (secretaria-task-clocked-time))
         :mode 'org-mode ))

(defun secretaria-task-clocked-out ()
  "Stop reminding the clocked-in task."
  (secretaria--task-delete-save-clocked-task)
  (ignore-errors (cancel-timer secretaria-clocked-in-reminder-timer))
  (when org-clock-current-task
    (alert (secretaria--org-to-html org-clock-current-task)
           :title (format "Task clocked out! (%s)" (secretaria-task-clocked-time))
           :severity 'high
           :mode 'org-mode)))

(defun secretaria-task-clocked-canceled ()
  "Stop reminding the clocked-in task if it's canceled."
  (cancel-timer secretaria-clocked-in-reminder-timer)
  (when org-clock-current-task
    (alert (secretaria--org-to-html org-clock-current-task)
           :title (format "Task canceled! (%s)" (secretaria-task-clocked-time))
           :severity 'high
           :mode 'org-mode)))

(defun secretaria-task-save-clocked-task ()
  "Save into a file the current clocked task."
  (when org-clock-current-task
    (with-temp-file (expand-file-name secretaria-clocked-task-save-file)
      (insert org-clock-current-task))))

(defun secretaria-task-load-clocked-task ()
  "Load the clocked task, if any.  And tell the user about it."
  (if (file-exists-p secretaria-clocked-task-save-file)
      (with-temp-buffer
        (insert-file-contents secretaria-clocked-task-save-file)
        (when (not (string-empty-p (buffer-string)))
          (alert (format "Something went wrong with Emacs while this task was clocked: <b>%s</b>" (buffer-string))
                 :title "Oops! Don't forget you were doing something, boss!"
                 :severity 'high)
          (secretaria--task-delete-save-clocked-task)))))

(defun secretaria--task-delete-save-clocked-task ()
  "Delete the saved clocked task."
  (ignore-errors (delete-file secretaria-clocked-task-save-file)))

(defun secretaria--task-saved-clocked-task-p ()
  "Check if the current clocked task was saved."
  (file-exists-p secretaria-clocked-task-save-file))

(add-hook 'org-clock-in-hook #'secretaria-task-clocked-in t)
(add-hook 'org-clock-out-hook #'secretaria-task-clocked-out t)
(add-hook 'org-clock-cancel-hook #'secretaria-task-clocked-canceled t)
(add-hook 'after-init-hook #'secretaria-task-load-clocked-task)

(when secretaria-notification-handler-overwrite
  (setf org-show-notification-handler 'secretaria-notification-handler))

(provide 'secretaria-clock)
;;; secretaria-clock.el ends here
