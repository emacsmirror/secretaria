Changelog
=========


(unreleased)
------------

Changes
~~~~~~~
- Updates org-mode version. [Jorge Araya Navarro]

Fix
~~~
- Remind the user only those tasks pending for the day. [Jorge Araya
  Navarro]

  If tasks where DONE they will be reminded to the user anyway


0.2.4 (2016-10-17)
------------------
- Changing package description as suggested by Steve. [Jorge Araya
  Navarro]
- Melpa badge and minor changes. [Jorge Araya Navarro]


0.2.3 (2016-10-16)
------------------
- Fixes "Selecting deleted buffer" error. [Jorge Araya Navarro]
- Downgrading dependent org-mode version. [Jorge Araya Navarro]


0.2.2 (2016-10-16)
------------------
- Applying those tips people on MELPA gave me. [Jorge Araya Navarro]


0.2.1 (2016-10-14)
------------------
- Visiting files first. [Jorge Araya Navarro]

  Because `Selecting deleted buffer` is caused when the user kills a
  buffer of files from `org-agenda-files`, I'm visiting the file first.


0.2 (2016-10-11)
----------------
- Using a variable for file location. [Jorge Araya Navarro]
- Merged in syohex/secretaria.el/fix-package (pull request #1) [Jorge
  Araya Navarro]

  Fix package
- Don't use free variable. [Syohei YOSHIDA]
- Load missing libraries. [Syohei YOSHIDA]
- Define package group and specify group for customize variables.
  [Syohei YOSHIDA]
- Adding an statement in the read me. [Jorge Araya Navarro]
- Fixing code conventions and dependencies. [Jorge Araya Navarro]
- Fixing image on read me. [Jorge Araya Navarro]
- Fixing read me file. [Jorge Araya Navarro]
- Specifying the required packages for this project. [Jorge Araya
  Navarro]
- Getting tasks for today with unknown time of day. [Jorge Araya
  Navarro]

  The user can get reminders every N minutes (default to 30) of these
  tasks so he will never forget them.


0.1.3 (2016-10-09)
------------------
- Saving clocked in task when clocked. [Jorge Araya Navarro]
- Checking if file exist before visiting it. [Jorge Araya Navarro]
- Useful functions added to after-init hook. [Jorge Araya Navarro]
- Updating the agenda. [Jorge Araya Navarro]

  When the user saves any modification done to any file from
  `org-agenda-file`, `org-agenda-appt` kicks in and updates.

  I'm working on getting tasks forgotten by the user, i.e.: re-scheduled
  by org-mode or with due deadlines. Hopefully someone more experienced
  answer my question http://emacs.stackexchange.com/q/27579/690
- Merge branch 'feature/custom-html-export' [Jorge Araya Navarro]
- Saving task clocked. [Jorge Araya Navarro]

  Useful later when Secretaria.el is checking your TO-DOs and stuff like
  that, and if your Emacs session crashed, she will remind you you were
  doing a task at that moment.
- Notification time passed and effort fixes bug #4. [Jorge Araya
  Navarro]

  Thanks to Yasushi Shoji for reviewing my messy code on Emacs stackexchange


0.1.2 (2016-02-15)
------------------
- Merge branch 'release/0.1.2' [Jorge Araya Navarro]
- There is no point on evaluating nonexistant values. [Jorge Araya
  Navarro]


0.1.1 (2016-02-14)
------------------
- Merge branch 'release/0.1.1' [Jorge Araya Navarro]
- Fixes bug #2. [Jorge Araya Navarro]

  Secretaria sets `org-show-notification-handler` if `nil` and also avoids
  doing this if the user disables this through `secretaria/notification-handler-overwrite`


0.1 (2016-02-14)
----------------
- Merge branch 'release/0.1' [Jorge Araya Navarro]
- Adding the Jesus prayer. [Jorge Araya Navarro]
- Fixes bug #1. [Jorge Araya Navarro]

  Secretaria will try to use the best notification style available so the
  user don't have to.
- Unlinking author of README image. [Jorge Araya Navarro]

  But leaving the copyright intact and in place.
- Bitbucket do not render HTML tags in README files. [Jorge Araya
  Navarro]
- Minor fix for README file. [Jorge Araya Navarro]
- A README file in markdown. [Jorge Araya Navarro]

  This will last until Bitbucket supports org-mode markup
- Clock-in and clock-out tasks. [Jorge Araya Navarro]

  Now Secretaria knows when you are working on something and remind you
  what you are doing.


