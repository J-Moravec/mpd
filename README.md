# mpd -- Minimal Package Development tools

`mpd` contains tools for package development, think about this as a very minimal barebone variant
of `devtools`.

## Aim

The aim is to support the very convenient loop `test()`, `document()`, and `check()`, but
by using as much base R as possible (except for the `roxygen2` package).

## Installation

Currently, `mpd` lives on github. To install `mpd`, type:

```

install.packages("https://github.com/J-Moravec/mpd/archive/refs/heads/master.tar.gz", repos = NULL)
```

or use packages such as `remotes`:

```
remotes::install_github("J-Moravec/mpd")
```

Or, to update to the latest dev version of `mpd`, you can use:

```
mpd::install_github("J-Moravec/mpd")
```


## Limitations

Currently both `test()` and `check()` builds the package. If your package includes any C, C++,
or other code requiring compilation, it will be compiled. This might take potentially a lot
of time if the compilation is particularly slow.
