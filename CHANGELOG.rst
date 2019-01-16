Changelog
=========


0.2.14 (2019-01-16)
-------------------

Changes
~~~~~~~
- Force undercover execution. [Jorge Araya Navarro]

Fix
~~~
- Alert user for due appts when they exists. [Jorge Araya Navarro]
- Don't forget to answer "yes" automatically. [Jorge Araya Navarro]
- Install curl. [Jorge Araya Navarro]


0.2.13 (2019-01-13)
-------------------

Changes
~~~~~~~
- Remove older emacs version from testing. [Jorge Araya Navarro]

  I cannot update org-mode with Cask in order to have the most recent version of org-mode, thus, I
  decided to remove them from the CI test. Instead is asummed that the user will have the most recent
  version of org-mode before installing secretaria.el

  - https://github.com/cask/cask/issues/119
  - https://github.com/cask/cask/issues/169
- Change gitlab-ci definition. [Jorge Araya Navarro]
- Change docker image and add compatibility tests. [Jorge Araya Navarro]

Fix
~~~
- Use script instead of passing a string as argument for command. [Jorge
  Araya Navarro]
- Include org as a dependency. [Jorge Araya Navarro]


0.2.12 (2019-01-12)
-------------------

Changes
~~~~~~~
- Bump version. [Jorge Araya Navarro]

Fix
~~~
- Require org-duration. [Jorge Araya Navarro]


0.2.11 (2019-01-12)
-------------------

New
~~~
- Implement more unit tests. [Jorge Araya Navarro]
- Set up codecov.io. [Jorge Araya Navarro]

Changes
~~~~~~~
- Upload code coverage conditionally. [Jorge Araya Navarro]
- Change text for alerts. [Jorge Araya Navarro]
- Update dependency `f` version and bump package version. [Jorge Araya
  Navarro]

Fix
~~~
- Hook in with the correct hooks. [Jorge Araya Navarro]
- Fix wrong math 🤦🏻🤦🏻 [Jorge Araya Navarro]
- Fix wrong let expression usage 🤦🏻 [Jorge Araya Navarro]
- Redirect output to bash command correctly. [Jorge Araya Navarro]

  - https://gitlab.com/shackra/secretaria/-/jobs/139907169
- Upload coverage information. [Jorge Araya Navarro]
- Remove compiled .el files before running tests. [Jorge Araya Navarro]

  This is for code coverage


0.2.10 (2018-10-25)
-------------------

Fix
~~~
- Use real English in description of features. [Jorge Araya Navarro]


0.2.9 (2018-10-12)
------------------

New
~~~
- Add GNU GPL 3 license. [Jorge Araya Navarro]

Changes
~~~~~~~
- Refactor function definition. [Jorge Araya Navarro]
- Add CI configuration. [Jorge Araya Navarro]
- Convert org markup to plain text by default. [Jorge Araya Navarro]

  Some notification displayers do not support HTML tags, others do. By default we deactivate the
  conversion of the markup to HTML.

  - https://people.gnome.org/%7Emccann/docs/notification-spec/notification-spec-latest.html
  - https://github.com/mumble-voip/mumble/issues/2211#issuecomment-213798387

Fix
~~~
- Reference correct variable name. [Jorge Araya Navarro]

  - https://gitlab.com/shackra/secretaria/-/jobs/107360104
- Fix readme file and comments on source code. [Jorge Araya Navarro]
- Merge files into one and fix typo. [Jorge Araya Navarro]

  - https://github.com/melpa/melpa/pull/5730#issuecomment-425614705
- Update function calls on tests. [Jorge Araya Navarro]

  Names were changed but test were left without update

  - https://gitlab.com/shackra/secretaria/-/jobs/97502461
- Fix issues reported by checkdoc. [Jorge Araya Navarro]
- Fix almost all issues reported by package-lint. [Jorge Araya Navarro]
- Include configuration file for ert-runner. [Jorge Araya Navarro]

  This file tells ert-runner to put the current directory in the load-path enabling any feature
  provided by the package under development.

  - https://github.com/rejeep/ert-runner.el/issues/28#issuecomment-239625263
- Install bash on docker image. [Jorge Araya Navarro]

  ert-runner seems to need bash installed on the environment

  - https://gitlab.com/shackra/secretaria/-/jobs/96450170
- Add unit testing. [Jorge Araya Navarro]

  Check for the health of the project and ensure the correct functioning of some key functions.


0.2.8 (2018-01-04)
------------------

Changes
~~~~~~~
- Display links as HTML for alerts. [Jorge Araya Navarro]


0.2.7 (2017-08-28)
------------------

Fix
~~~
- Do not skip `todo` entries. [Jorge Araya Navarro]

  `org-agenda-skip-entry-if` skip an entry *if any of the conditions are true*.

  refs #8


0.2.6 (2017-08-28)
------------------

Changes
~~~~~~~
- Avoid reminding currently clocked task. [Jorge Araya Navarro]

  Makes no sense to remind the user about a task they are currently working in

Fix
~~~
- Sets `org-clock-current-task' to empty string if nil. [Jorge Araya
  Navarro]

  Avoid conflicts of types with `string-match-p'
- Stop reminding about DONE tasks for today. [Jorge Araya Navarro]

  fixes #8


0.2.5 (2017-08-13)
------------------

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


