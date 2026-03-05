# renvvv 0.1.1

* Initial CRAN submission.
* `renvvv_restore()`: Restores packages from the lockfile, continuing past
  individual failures and retrying them individually.
* `renvvv_update()`: Updates packages to their latest available versions,
  continuing past individual failures.
* `renvvv_restore_and_update()`: Combines restore and update in sequence.
