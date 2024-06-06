# Cplot

Cplot is an R package for creating customized, publish-ready plot. It's under construction...

## Installation

You can install the latest version of Cplot from GitHub using `devtools`:

```r
devtools::install_github("CTKlab/Cplot")
```

## Usage

To create a scatter plot with correlation analysis, use the `Cplot.scatter` function:

```r
library(Cplot)

# Create a data frame
df <- data.frame(x = rnorm(100), y = rnorm(100))

# Create a scatter plot
Cplot.scatter(df, "x", "y", title = "Scatter Plot", subtitle = "Sample Data")
```

For more information on how to use the package, please refer to the documentation.

## Contributing

Contributions are welcome! If you find any issues or have suggestions for improvements, please open an issue or submit a pull request on GitHub.

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
