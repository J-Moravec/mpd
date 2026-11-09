# TODO

- [ ] Update `install_dependencies()` and dependency parsing in
  `.parse_dependencies()` (`R/packages.r`) to account for version requirements
  from `DESCRIPTION`: an installed package that is too old must not be skipped.
- [ ] Update `package_dependencies()` (`R/packages.r`) to read a local package
  directory's `DESCRIPTION` instead of an installed package with the same name,
  including for relative paths.
- [ ] Document the `pkg` argument for `install_dependencies()` in `R/packages.r`
  and regenerate its help page.
- [ ] Test `install_dependencies()` without network access: missing dependencies
  are requested, already-satisfied dependencies are skipped, outdated
  dependencies are not skipped, and an empty dependency list does not trigger
  an installation.
- [ ] Test `package_dependencies()` selection (`strong`, `most`, `all`, and
  explicit fields), `install()` (`R/mpd.r`) with its default and
  `dependencies = NULL`, and local paths that collide with installed names.
