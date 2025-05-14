# mpd 0.0.3

* added `install()` for local installation
    - A common operation when the pkg is developed to solve an immediate problem
    - `install.packages(".", repos = NULL)` is wordy and doesn't unload the pkg from namespace,
      so the old version is still hanging around

* fix `test()`, previously all files in the `tests` directory were globbed, now only the R files are (.r and .R extensions)

# mpd 0.0.2

* added `install_github()` to install a pkg from github

# mpd 0.0.1

* Initial version of the package
* support the `test()`, `document()` and `check()` development loop
