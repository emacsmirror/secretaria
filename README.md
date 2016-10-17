![A pretty and busy office lady](http://i.imgur.com/NginR7g.png)

[![MELPA](http://melpa.milkbox.net/packages/secretaria-badge.svg)](http://melpa.milkbox.net/#/secretaria)

# What's this?

A personal assistant based on org-mode. This package contains utilities that enhance your experience
with org-mode.

# Features

- Reminders
  - All tasks scheduled or that have a deadline set to "today", but have no time of the day
    specified.
  - The current clocked task, every N minutes (10 by default).
  - In case of Emacs crashing, the task clocked in at the moment so you don't forget about fixing
    that.

# How to use

This package should be available in Melpa, if you use `use-package`, throw this code snippet in your
Emacs configuration.

```
(use-package secretaria
  :config
  ;; use this for getting a reminder every 30 minutes of those tasks scheduled
  ;; for today and which have no time of day defined.
  (add-hook 'after-init-hook #'secretaria-today-unknown-time-appt-always-remind-me))
```

# About boycotting packages on non-technical issues

![Don't tread on Emacs](https://alphapapa.github.io/dont-tread-on-emacs/dont-tread-on-emacs-150.png)

No Emacs-related project should exclude people, boycott other projects, or discourage participation in other projects because of any difference of opinion over non-technical matters. Such divisiveness weakens the community, discourages participation in the Emacs community, and holds back progress in improving software. [Don't tread on Emacs](https://alphapapa.github.io/dont-tread-on-emacs/).
