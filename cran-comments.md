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
* Corrects memoise issue following <https://github.com/r-lib/memoise/issues/76>. This introduces the use of `<<-` to modify the package namespace. This does not modify the global environment.
