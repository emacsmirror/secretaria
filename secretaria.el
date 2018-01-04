;;; secretaria.el --- A personal assistant based on org-mode

;; Copyright (C) 2016-2018 Jorge Araya Navarro

;; Author: Jorge Araya Navarro <jorge@esavara.cr>
;; Keywords: org, convenience
;; Package-Requires: ((emacs "24.4") (alert "1.2") (s "1.12") (f "0.19.0"))
;; Package-Version: 0.2.8
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

;; # What's this?
;;
;; A personal assistant based on org-mode.  This package contains utilities that
;; enhance your experience with org-mode.
;;
;; # Features
;;
;; - Reminders
;; - All tasks scheduled or that have a deadline set to "today", but have no time of the day
;; specified.
;; - The current clocked task, every N minutes (10 by default).
;; - In case of Emacs crashing, the task clocked in at the moment so you don't forget about fixing
;; that.
;;
;; # How to use
;;
;; This package should be available in Melpa, if you use `use-package`, throw this code snippet in your
;; Emacs configuration.
;;
;;     (use-package secretaria
;;       :config
;;       ;; use this for getting a reminder every 30 minutes of those tasks scheduled
;;       ;; for today and which have no time of day defined.
;;       (add-hook 'after-init-hook #'secretaria-today-unknown-time-appt-always-remind-me))

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

(require 'secretaria-appt)
(require 'secretaria-clock)

(defgroup secretaria nil
  "A personal assistant based on org-mode"
  :group 'org)

(provide 'secretaria)
;;; secretaria.el ends here
