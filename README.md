# plotibcs

### Data-driven helper functions to quickly produce IBCS-compliant charts

## Installation

You can install the development version from this repo:

```r
# install.packages("devtools")
devtools::install_github("ncalm/plotibcs")
```

## Included functions

1. plot_ibcs_waterfall

The purpose of this function is to mimic chart type 12 described on the ibcs website [here](https://www.ibcs.com/resource/chart-template-12/)

One example given is this:
![Example of C12](https://www.ibcs.com/wp-content/uploads/2016/08/IBCS_chart_template_12-1.png)

My intent with this function is to recreate the vertical waterfall from that template with a simple data-driven interface, where the user can specify exactly what order they want bars to appear, how bars are related to one another, where dividing lines are drawn and which labels are bolded. 

Formatting related to bar colors, font size and positioning of labels is pre-defined by the function, the idea being that the user should just get a function that fits the template.
