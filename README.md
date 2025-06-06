# plotibcs

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)


### Data-driven helper functions to quickly produce IBCS-compliant charts

## Installation

You can install the development version from this repo:

```r
# install.packages("devtools")
devtools::install_github("ncalm/plotibcs")
```

## License

This package is licensed under the **GNU General Public License v3.0 (GPL-3)**.

You are free to use, modify, and redistribute the software under the terms of the license. Any derivative works must also be released under GPL-3.

See the [LICENSE](LICENSE.md) file for full details.


## Included functions

1. plot_ibcs_waterfall

The purpose of this function is to mimic chart type 12 described on the ibcs website [here](https://www.ibcs.com/resource/chart-template-12/)

One example given is this:

<img src="https://www.ibcs.com/wp-content/uploads/2016/08/IBCS_chart_template_12-1.png" alt="IBCS C12 Template" width="50%" />

My intent with this function is to recreate the vertical waterfall from that template with a minimal amount of setup for the user. 

As an example, this table:

<img src="images/table.png" alt="Example C12 table" width="50%" />

Which must be defined as follows:
- position (integer) non null
- type (character) non null, taking one of these values:
  - bar, for drawing a bar for a line item
  - subtotal, which positions a subtotal for a collection of bars (see subtotal_group)
  - divider, which draws a horizontal line between two adjacent bars
  - result, which draws a bar from zero to the end value of the prior bar
- bold (logical) non null, indicating if the labels for this bar should be bold
- subtotal_group (integer), indicating which bars should be summed together. Generally each group should have one or more rows with type='bar' and exactly one row with type='subtotal'. Nulls in this column will not contribute to any subtotals. Similarly, subtotal rows with no group will not be drawn.
- One character column of bar labels. Nulls are allowed on divider rows.
- One numeric column of line item values. You need only provide values for rows with type 'bar' since subtotals and results are computed by the function.

Is passed into the function like this:

```r
# df is a tibble of the table shown above
plot_ibcs_waterfall(df, title = "Previous Year", value_col = "PY", label_col = "Line Item")
```

And produces this chart:

<img src="images/waterfall.png" alt="Example C12 chart from table" width="50%" />

2. plot_ibcs_variance

The purpose of this function is to mimic the two variance charts shown on the ibcs website [here](https://www.ibcs.com/resource/chart-template-12/)

Setting var_type to "abs" (the default) will produce a chart that looks like the left-most green and red chart in the image below. 

<img src="https://www.ibcs.com/wp-content/uploads/2016/08/IBCS_chart_template_12-1.png" alt="IBCS C12 Template" width="50%" />

To produce the right-most chart, set var_type to "pct".

Note that in these charts, "bad" measurements are coloured red and "good" measurements are coloured green, regardless of the sign of the measurement. For example, an increase in an expense is a positive value but a bad outcome, so it is a right-facing red bar. Similarly, a decrease of an expense is a good outcome, so it's a left-facing green bar.

Similarly to the waterfall chart, the first argument is a data.frame, which must be defined as follows:
- position (integer) non null
- type (character) non null, taking one of these values:
  - bar, for drawing a bar for a line item
  - subtotal, which positions a subtotal for a collection of bars (see subtotal_group)
  - divider, which draws a horizontal line between two adjacent bars
  - result, which draws a bar from zero to the end value of the prior bar
- bold (logical) non null, indicating if the labels for this bar should be bold
- subtotal_group (integer), indicating which bars should be summed together. Generally each group should have one or more rows with type='bar' and exactly one row with type='subtotal'. Nulls in this column will not contribute to any subtotals. Similarly, subtotal rows with no group will not be drawn.
- One character column of bar labels. Nulls are allowed on divider rows.
- invert (logical) non null, indicating if the absolute value of this bar should be inverted when calculated and coloured. Generally, expenses should be flagged as true and income as false, but similar arguments apply to non-financial use of this chart.
- Two numeric column of line item values, one representing the reported value of a 'before' state (such as previous year) and one representing the reported value of an 'after' state (such as actuals or currenty year). You need only provide values for rows with type 'bar' since subtotals and results are computed by the function. Note that these columns are *not* the calculated values of the variance. Rather, the variance is calculate from them.

Some examples of usage:

```r
# df is a tibble of the table shown above, this function call calculates and displays the absolute variance from PY to AC
plot_ibcs_variance(df, title = "Abs. Var.", from_col = "PY", to_col = "AC", label_col = "Line Item", var_type = "abs")

# this function call calculates and displays the percentage variance from PY to AC
plot_ibcs_variance(df, title = "Pct. Var.", from_col = "PY", to_col = "AC", label_col = "Line Item", var_type = "pct")
```

In addition to using these charts in standard R scripts and in RStudio, you can use Anaconda Code from the Anaconda Toolbox for Excel add-in to run R directly in Excel. You are welcome to explore how these functions work using [this example file](examples/plotibcs_plot_ibcs_waterfall.xlsx).

For more details on using R in Excel, see [this blog post](https://www.anaconda.com/blog/anaconda-code-brings-r-to-excel).

To install Anaconda Toolbox, select the Add-Ins button from the Home tab of the Excel ribbon and search for 'AnacondaToolbox'.
