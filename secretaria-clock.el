;;; secretaria-clock.el --- Org-mode clock related enhance

;; Copyright (C) 2016 Jorge Araya Navarro

;; Author: Jorge Araya Navarro <elcorreo@deshackra.com>
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

;;; Code:

(require 'org)
(require 'alert)

(defun secretaria/style--get-best-available ()
  "Return the best style available for the current system"
  (cond ((executable-find "growlnotify") 'growl)
        ((executable-find "terminal-notifier") 'notifier)
        ((stringp dbus-compiled-version) 'dbus)
        ((executable-find "notify-send") 'libnotify)))

(defcustom secretaria/remind-every-minutes 10 "How many minutes remind the user of the task clocked in" :type 'integer)
(defcustom secretaria/style-best-available (secretaria/style--get-best-available) "Use the best notification style available for the current operating system" :type 'symbol)

(defun secretaria/remind-task-clocked-in ()
  "Fires an alert for the user reminding him which task he is working on"
  (when org-clock-current-task
    (if (not org-clock-task-overrun)
        (alert (format "%s" org-clock-current-task) :title "Currently clocked" :severity 'trivial :mode 'org-mode :style secretaria/style-best-available)
      (alert (format "%s" org-clock-current-task) :title "Task time exceeded!" :severity 'urgent :mode 'org-mode :style secretaria/style-best-available))))

(defun secretaria/task-clocked-in ()
  "Start a timer when a task is clocked-in"
  (setf secretaria/remind--timer (run-at-time (format "%s min" secretaria/remind-every-minutes) (* secretaria/remind-every-minutes 60) 'secretaria/remind-task-clocked-in)))

(defun secretaria/task-clocked-out ()
  "Stop reminding the clocked-in task"
  (ignore-errors (cancel-timer secretaria/remind--timer))
  (when org-clock-current-task
    (alert (format "%s" org-clock-current-task) :title "Task clocked out!" :severity 'high :mode 'org-mode :style secretaria/style-best-available)))

(defun secretaria/task-clocked-canceled ()
  "Stop reminding the clocked-in task if it canceled"
  (cancel-timer secretaria/remind--timer)
  (when org-clock-current-task
    (alert (format "%s" org-clock-current-task) :title "Task canceled!" :severity 'high :mode 'org-mode :style secretaria/style-best-available)))

(add-hook 'org-clock-in-hook 'secretaria/task-clocked-in t)
(add-hook 'org-clock-out-hook 'secretaria/task-clocked-out t)
(add-hook 'org-clock-cancel-hook 'secretaria/task-clocked-canceled t)

(provide 'secretaria-clock)
