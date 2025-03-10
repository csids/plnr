# plnr <a href="https://www.csids.no/plnr/"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[plnr](https://www.csids.no/plnr/) is a framework for planning analyses within the mental model where you have one (or more) datasets and want to run either:

- The same function multiple times with different arguments.
- Multiple functions. 
    
This is appropriate when:

- Applying the same function to multiple strata (e.g., locations, age groups).
- Applying the same statistical method to multiple variables (e.g. exposures).
- Creating multiple different tables or graphs for a report.

Read the introduction vignette [here](https://www.csids.no/plnr/articles/plnr.html) or run `help(package="plnr")`.

## csverse

The [csverse](https://www.csids.no/packages.html) is a set of R packages developed to help solve problems that frequently occur when performing disease surveillance.

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

