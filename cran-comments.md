## Test environments
* local R installation (Windows 10), R 4.1.1
* local R installation (macOS 11.4), R 4.1.1
* ubuntu 20.04 (on GitHub Actions), (devel and release)
* windows-latest (on GitHub Actions), (release)
* macOS-latest (on GitHub Actions), (release)
* rhub::check_on_solaris()

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Additional Submission Notes
* UTF-8 issue should be fixed. Replaced a remaining UTF character with ASCII.
* Existing note for memoise appears to be a false positive, as it is used in two functions.
