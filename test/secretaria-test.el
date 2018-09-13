;;; secretaria-test -- Test for `secretaria'

;;; Commentary:
;; These are the tests for `secretaria'

;;; Code:

(ert-deftest secretaria-file-empty ()
  "Should fail because the agenda file is empty."
  (let ((org-test-file-empty t))
    (within-sandbox
     (should-not (secretaria-appt-get-appt 'due))
     (should-not (secretaria-appt-get-appt 'unknown)))))

(ert-deftest secretaria-due-appts ()
  "Should find due appointments."
  (within-sandbox
   (should (equal (length (secretaria-appt-get-appt 'due)) 3))))

(ert-deftest secretaria-unknown-time ()
  "Should find appointments for today in unspecified time of day."
  (within-sandbox
   (should (equal (length (secretaria-appt-get-appt 'unknown)) 3))))
