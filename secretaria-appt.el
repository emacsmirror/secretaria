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

;;; Code:

(require 'f)

(defun secretaria/org-file-agenda-p (filename-directory)
  "Return t if FILENAME-DIRECTORY is in `org-agenda-files'"
  (when (not (eq filename-directory nil))
    (if (listp org-agenda-files)
        ;; do something when file names are stored inside another file
        (or (member filename-directory org-agenda-files)
           (file-in-directory-p (file-name-nondirectory filename-directory) org-directory)))))

(defun secretaria/update-appt ()
  "Update appointments if the saved file is part of the agendas files"
  (interactive)
  (when (eq major-mode 'org-mode)
    ;; (file-name-directory (buffer-file-name))
    (when (secretaria/org-file-agenda-p buffer-file-name)
      ;; TODO: do something interesting
      (message "File %s is an org-mode agenda file!" buffer-file-name))))
