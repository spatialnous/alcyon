## alcyon
An R package for spatial network analysis. Based on the rdepthmap package and leveraging the sala library from depthmapX

### How to install:
```
install.packages("alcyon")
```
Windows users might need to also install [Rtools](https://cran.r-project.org/bin/windows/Rtools/)

A demonstration of the package took place in the “Space syntax analysis in R” workshop of the 12th International Space Syntax Symposium (SSS12) in China on the 8th of July 2019, the content of which you may find [here](https://github.com/pklampros/space_syntax_analysis_in_r).

### Development:
Make sure that git hooks are enabled, to avoid committing minor issues:
```
git config --local core.hooksPath .githooks/
```

To be able to do all development tasks (including testing and linting) all these packages are necessary:
```
install.packages(c('sf', 'stars', 'rmarkdown', 'devtools', 'spelling', 'lintr', 'Rcpp', 'knitr', 'testthat', 'withr', 'cyclocomp', 'httr'))
```

Do not change `configure` directly, instead change `configure.ac` and run `autoconf`

To force enable OpenMP if R was not built with it, install the source package with:
```
install.packages("alcyon", configure.args = "--enable-force-openmp")

```
