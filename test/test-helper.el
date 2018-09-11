;;; test-helper --- Test helper for secretaria

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar secretaria-test-path
  (f-dirname (f-this-file)))

(defvar secretaria-root-path
  (f-parent secretaria-test-path))

(defvar secretaria-sandbox-path
  (f-expand "sandbox" secretaria-test-path))

(defvar org-test-file (expand-file-name "file.org" secretaria-sandbox-path))

(defvar org-test-file-empty nil)

(setf org-agenda-files `(,org-test-file))

(defvar org-file-body
  "
* %s
%s %s")

(defun helper-time-calc (amount &optional unspecified)
  "Add AMOUNT of hours to the current time, return a time stamp.

When UNSPECIFIED is non-nil the time stamp is return with out time of the day"
  (let ((the-time (if (< amount 0)
                      (time-subtract (current-time) (seconds-to-time (* 60 60 (* -1 amount))))
                    (time-add (current-time) (seconds-to-time (* 60 60 amount))))))
    (if unspecified
        (format-time-string "<%Y-%m-%d %a>" the-time)
      (format-time-string "<%Y-%m-%d %a %H:%M>" the-time))))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory secretaria-sandbox-path))
     (when (f-exists? secretaria-sandbox-path)
       (f-delete default-directory :force))
     (when (get-buffer (f-filename org-test-file))
       (kill-buffer (f-filename org-test-file)))
     (f-mkdir secretaria-sandbox-path)
     (with-current-buffer (find-file-noselect org-test-file)
       (when (not org-test-file-empty)
         (insert (format org-file-body "yesterday 1" "SCHEDULED:" (helper-time-calc -24)))
         (insert (format org-file-body "yesterday 2" "DEADLINE:" (helper-time-calc -48)))
         (insert (format org-file-body "yesterday unknown 1" "SCHEDULED:" (helper-time-calc -48 t)))
         (insert (format org-file-body "today unknown 1" "SCHEDULED:" (helper-time-calc 0 t)))
         (insert (format org-file-body "today unknown 2" "SCHEDULED:" (helper-time-calc 0 t)))
         (insert (format org-file-body "today unknown 3" "DEADLINE:" (helper-time-calc 0 t))))
       (save-buffer))
     ,@body))

(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require 'secretaria)

;;; test-helper.el ends here
