# plnr 

test test

## fhiverse

The `fhiverse` is a set of R packages developed by the Norwegian Institute of Public Health to help solve problems relating to:

- structural data in Norway (e.g. maps, population, redistricting)
- convenience functions for Norwegian researchers (e.g. Norwgian formatting, Norwegian characters)
- analysis planning (especially for making graphs/tables for reports)
- file structures in projects
- styleguides/recommendations for FHI employees

If you want to install the dev versions (or access packages that haven't been released on CRAN), run `usethis::edit_r_profile()` to edit your `.Rprofile`. Then write in:

```
options(repos=structure(c(
  FHI="https://folkehelseinstituttet.github.io/drat/",
  CRAN="https://cran.rstudio.com"
)))
```

Save the file and restart R. This will allow you to install `fhiverse` packages from the FHI registry.

Current `fhiverse` packages are:

- [spread](https://folkehelseinstituttet.github.io/spread)
- [fhidata](https://folkehelseinstituttet.github.io/fhi)
- [fhiplot](https://folkehelseinstituttet.github.io/fhi)
- [plnr](https://folkehelseinstituttet.github.io/fhi)
- [fhi](https://folkehelseinstituttet.github.io/fhi)
