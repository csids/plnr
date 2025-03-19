# plnr <a href="https://www.csids.no/plnr/"><img src="man/figures/logo.png" align="right" width="120" /></a>

## Overview 

[plnr](https://www.csids.no/plnr/) is a powerful framework for planning and executing analyses in R. It's designed for scenarios where you need to:

- Run the same function multiple times with different arguments
- Execute multiple different functions on the same datasets
- Create systematic analyses across multiple strata or variables

### Key Features

- **Efficient Data Management**: Load data once and reuse across multiple analyses
- **Structured Analysis Planning**: Organize analyses into clear, maintainable plans
- **Flexible Execution**: Support for both single-function and multi-function analysis plans
- **Built-in Debugging**: Convenient tools for development and testing
- **Parallel Processing**: Optional parallel execution of analyses
- **Hash-based Caching**: Track data changes and optimize execution

### Common Use Cases

- Applying the same analysis across multiple strata (e.g., locations, age groups)
- Running statistical methods on multiple variables (e.g., exposures, outcomes)
- Generating multiple tables or graphs for reports
- Creating systematic surveillance analyses

## Installation

### From CRAN

```r
install.packages("plnr")
```

### Development Version

To install the development version or access packages that haven't been released on CRAN:

1. Edit your `.Rprofile`:
```r
usethis::edit_r_profile()
```

2. Add the following configuration:
```r
options(
  repos = structure(c(
    CSVERSE = "https://www.csids.no/drat/",
    CRAN    = "https://cran.rstudio.com"
  ))
)
```

3. Restart R and install:
```r
install.packages("plnr")
```

## Getting Started

1. Read the [introduction vignette](https://www.csids.no/plnr/articles/plnr.html)
2. Check out the [adding analyses guide](https://www.csids.no/plnr/articles/adding_analyses.html)
3. Run `help(package="plnr")` for detailed function documentation

## Quick Example

```r
library(plnr)
library(ggplot2)
library(data.table)

# Create a new plan
p <- Plan$new()

# Add data
p$add_data(
  name = "deaths",
  direct = data.table(deaths=1:4, year=2001:2004)
)

# Add analyses for different years
p$add_argset(name = "fig_1_2002", year_max = 2002)
p$add_argset(name = "fig_1_2003", year_max = 2003)

# Define analysis function
fn_fig_1 <- function(data, argset) {
  plot_data <- data$deaths[year <= argset$year_max]
  ggplot(plot_data, aes(x=year, y=deaths)) +
    geom_line() +
    geom_point(size=3) +
    labs(title = glue::glue("Deaths from 2001 until {argset$year_max}"))
}

# Apply function to all argsets
p$apply_action_fn_to_all_argsets(fn_name = "fn_fig_1")

# Run analyses
p$run_one("fig_1_2002")
```

## Documentation

- [Package Website](https://www.csids.no/plnr/)
- [GitHub Repository](https://github.com/csids/plnr)
- [Issue Tracker](https://github.com/csids/plnr/issues)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This package is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
