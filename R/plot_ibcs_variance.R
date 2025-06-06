#' Create an IBCS-style variance chart
#'
#' Generates a vertical variance chart according to IBCS standards from a properly structured data frame.
#'
#' @param df_raw A data frame or tibble containing the raw chart data.
#' @param title Title of the chart.
#' @param from_col Name of the column containing numeric values that represents the first period.
#' @param to_col Name of the column containing numeric values that represents the second period.
#' @param label_col Name of the column containing line item labels.
#' @param var_type Name of the column containing the requested variance type (abs or pct)
#'
#' @return A ggplot object
#' @import dplyr
#' @import ggplot2
#' @export
plot_ibcs_variance <- function(
    df_raw,
    title = "Var.",
    from_col = "Value1",
    to_col = "Value2",
    label_col = "Line.Item",
    var_type = "abs"
) {

  # Rename user-specified columns to fixed internal names
  df_raw <- df_raw %>%
    rename(
      Value1 = all_of(from_col),
      Value2 = all_of(to_col),
      `Line.Item` = all_of(label_col)
    )

  cols <- c("position", "type", "bold", "subtotal_group", "Line.Item", "invert", "Value1", "Value2")
  df_raw <- df_raw %>% select(all_of(cols))

  # Flag the rows that will be for plotting bars
  df <- df_raw %>% mutate(is_plot_row = type != "divider")

  # Assign vertical positions only to non-divider rows
  df$x[df$type != "divider"] <- seq_len(sum(df$type != "divider"))

  # Compute start/end for all types
  # anchor is always zero for a variance chart, on all bar types (bar, subtotal and result)
  anchor <- 0
  df <- df %>%
    mutate(
      end = if (var_type == "abs") {
        (Value2 - Value1) * if_else(invert, -1, 1)
      } else {
        (100 * (Value2 - Value1) / Value1)
      },
      start = anchor
    )



  for (i in seq_len(nrow(df))) {
    row_type <- df$type[i]
    invert <- df$invert[i]
    if (row_type == "subtotal") {
      group_id <- df$subtotal_group[i]

      bar_rows <- which(df$subtotal_group == group_id & df$type == "bar")
      if (length(bar_rows) > 0) {
        val1 <- sum(df$Value1[bar_rows], na.rm = TRUE)
        val2 <- sum(df$Value2[bar_rows], na.rm = TRUE)
        df$Value1[i] <- val1
        df$Value2[i] <- val2
        df$end[i] = if (var_type == "abs") {
          (val2 - val1) * if_else(invert, -1, 1)
        } else {
          (100 * (val2 - val1) / val1)
        }
      }

    } else if (row_type == "result") {
      # Get all rows above this one that are bars
      bar_rows_above <- df[1:(i - 1), ] %>% filter(type == "bar")
      total_var <- if (var_type == "abs") {
        sum(bar_rows_above$end * if_else(bar_rows_above$invert, -1, 1), na.rm = TRUE)
      } else {
        val1 <- sum(bar_rows_above$Value1)
        (100 * (sum(bar_rows_above$Value2) - val1) / val1) * if_else(invert, -1, 1)
      }

      df$start[i] <- anchor
      df$end[i] <- total_var

    }
  }

  bad_color <- "indianred3"   # softer than "red"
  good_color <- "chartreuse3"  # softer than "green"

  # Assign fill colors to drawn bars
  # Positive bars are gray with 70% transparency
  # Negative bars are gray with 30% transparency
  df <- df %>%
    mutate(
      fill = case_when(
        type == "divider" ~ NA_character_,
        is.na(end) ~ NA_character_,
        invert & end >= 0 ~ bad_color,       # Expense ↑ → bad
        invert & end < 0 ~ good_color,      # Expense ↓ → good
        !invert & end >= 0 ~ good_color,    # Income ↑ → good
        TRUE ~ bad_color                     # Income ↓ → bad
      )
    )


  # Format the bar labels
  # Currently zero decimal places
  # Negative bars have their labels to the left of the bar
  # Positive bars have their labels to the right of the bar
  # Negative bars don't use the minus symbol
  df_labels <- df %>%
    filter(type != "divider") %>%
    mutate(
      label = formatC(round(end, 0), format = "f", digits = 0, flag = "+"),
      label_x = if_else(end >= 0, end + 5, end - 5),
      hjust = if_else(end >= 0, 0, 1)
    )

  # this will be used to extend the x-axis to allow space for bar labels
  # x_max <- max(df_labels$label_x, na.rm = TRUE) + 100  # 10 is padding

  # divider_ys is a numeric vector of vertical positions
  # at which horizontal lines will be drawn to separate
  # bars from one another at the user's discretion
  divider_ys <- df %>%
    mutate(idx = row_number()) %>%
    filter(type == "divider") %>%
    mutate(
      prev_x = df$x[idx - 1],
      next_x = df$x[idx + 1],
      y = (prev_x + next_x) / 2
    ) %>%
    pull(y) # extracts this tibble column to a vector

  ggplot(df %>% filter(type != "divider")) +
    # draw the bars
    geom_rect(aes(ymin = x - 0.3, ymax = x + 0.3, xmin = start, xmax = end, fill = fill),
              color = NA) +
    # draw the bar labels
    geom_text(data = df_labels,
              aes(x = label_x, y = x, label = label, hjust = hjust, fontface = ifelse(bold, "bold", "plain")),
              size = 3) +
    # draw the dividing lines
    geom_segment(
      data = tibble(y = divider_ys),
      aes(x = -Inf, xend = Inf, y = y, yend = y),
      linewidth = 0.2,
      color = "black",
      inherit.aes = FALSE
    ) +
    # draw a vertical line at x=0
    geom_vline(xintercept = 0, color = "black", linewidth = 0.2) +
    # ensure that the bars are all plotted on the y-axis
    # this has the effect of rotating the bar plot from vertical to horizontal bars
    scale_y_reverse(
      breaks = df$x[df$type != "divider"],
      labels = function(labs) {
        idx <- match(labs, df$x)
        mapply(function(text, bold) {
          if (isTRUE(bold)) parse(text = paste0("bold('", text, "')"))
          else text
        }, df$`Line.Item`[idx], df$bold[idx])
      }
    ) +
    coord_cartesian(clip = "off") +
    # scale_x_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, x_max)) +
    scale_fill_identity() +
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}

