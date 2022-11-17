# plnr <a href="https://www.csids.no/plnr/"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[plnr](https://www.csids.no/plnr/) is a system to help you plan analyses.

Read the introduction vignette [here](https://www.csids.no/plnr/articles/plnr.html) or run `help(package="plnr")`.

## csverse

<a href="https://www.csids.no/packages.html"><img src="https://www.csids.no/packages/csverse.png" align="right" width="120" /></a>

The [csverse](https://www.csids.no/packages.html) is a set of R packages developed to help solve problems that frequently occur when performing infectious disease surveillance.

If you want to install the dev versions (or access packages that haven't been released on CRAN), run `usethis::edit_r_profile()` to edit your `.Rprofile`. 

Then write in:

```
options(
  repos = structure(c(
    CSVERSE = "https://www.csids.no/drat/",
    CRAN    = "https://cran.rstudio.com"
  ))
)
```

Save the file and restart R.

You can now install [csverse](https://www.csids.no/packages.html) packages from our [drat repository](https://www.csids.no/drat/).

```
install.packages("plnr")
```

