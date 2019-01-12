;;; secretaria-test -- Test for `secretaria'

;;; Commentary:
;; These are the tests for `secretaria'

;;; Code:

(ert-deftest secretaria-file-empty ()
  "Should fail because the agenda file is empty."
  (let ((org-test-file-empty t))
    (within-sandbox
     (should-not (secretaria-get-appt 'due))
     (should-not (secretaria-get-appt 'unknown)))))

(ert-deftest secretaria-due-appts ()
  "Should find due appointments."
  (within-sandbox
   (should (equal (length (secretaria-get-appt 'due)) 3))))

(ert-deftest secretaria-unknown-time ()
  "Should find appointments for today in unspecified time of day."
  (within-sandbox
   (should (equal (length (secretaria-get-appt 'unknown)) 3))))

(ert-deftest secretaria-due-appts-fail ()
  "Should fail because there are appointments."
  (within-sandbox
   (should-not (equal (length (secretaria-get-appt 'due)) 0))))

(ert-deftest secretaria-unknown-time ()
  "Should fail because there are appointments for today in unspecified time of day."
  (within-sandbox
   (should-not (equal (length (secretaria-get-appt 'unknown)) 0))))

(ert-deftest secretaria-task-clocked-time ()
  "Should return a string with the correct clocked time of a task"
  (let ((org-clock-start-time (current-time))
        (org-clock-total-time 73))
    (should (equal (secretaria-task-clocked-time) "1:13/0:00"))))

(provide 'secretaria-test)
;;; secretaria-test.el ends here
