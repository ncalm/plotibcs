#' Create an IBCS-style vertical waterfall chart
#'
#' Generates a vertical waterfall chart according to IBCS standards from a properly structured data frame.
#'
#' @param df_raw A data frame or tibble containing the raw chart data.
#' @param title Title of the chart.
#' @param value_col Name of the column containing numeric values.
#' @param label_col Name of the column containing line item labels.
#'
#' @return A ggplot object
#' @import dplyr
#' @import ggplot2
#' @export
plot_ibcs_waterfall <- function(
    df_raw,
    title = "Waterfall Chart",
    value_col = "Value",
    label_col = "Line.Item"
) {

  # Rename user-specified columns to fixed internal names
  df_raw <- df_raw %>%
    rename(
      Value = all_of(value_col),
      `Line.Item` = all_of(label_col)
    )

  cols <- c("position", "type", "bold", "subtotal_group", "Line.Item", "Value")
  df_raw <- df_raw %>% select(all_of(cols))

  # Flag the rows that will be for plotting bars
  df <- df_raw %>% mutate(is_plot_row = type != "divider")

  # Assign vertical positions only to non-divider rows
  df$x[df$type != "divider"] <- seq_len(sum(df$type != "divider"))

  # Compute start/end for all types
  anchor <- 0
  for (i in seq_len(nrow(df))) {
    row_type <- df$type[i]

    if (row_type == "bar") {
      df$start[i] <- anchor
      df$end[i] <- anchor + df$Value[i]
      anchor <- df$end[i]

    } else if (row_type == "subtotal") {
      group_id <- df$subtotal_group[i]
      bar_rows <- which(df$subtotal_group == group_id & df$type == "bar")
      if (length(bar_rows) > 0) {
        subtotal_value <- sum(df$Value[bar_rows], na.rm = TRUE)
        df$Value[i] <- subtotal_value
        group_start <- df$start[bar_rows[1]]
        df$start[i] <- group_start
        df$end[i] <- group_start + subtotal_value
        anchor <- df$end[i]
      }

    } else if (row_type == "result") {
      df$start[i] <- 0
      df$end[i] <- anchor
      df$Value[i] <- anchor
      anchor <- df$end[i]
    }
  }

  # calculate the coordinates for non-subtotal and non-result connecting lines
  df_lines <- df %>%
    filter(type != "divider") %>%
    arrange(x) %>%
    mutate(
      x_next = lead(x),
      xend = lead(start)
    ) %>%
    filter(!is.na(x_next)) %>%
    transmute(
      y = x - 0.3, # from the vertical top of the current bar
      yend = x_next + 0.3, # to the vertical bottom of the next bar
      x = xend,
      xend
    )

  # Assign fill colors to drawn bars
  # Positive bars are gray with 70% transparency
  # Negative bars are gray with 30% transparency
  df <- df %>%
    mutate(
      fill = case_when(
        type == "divider" ~ NA_character_,
        Value >= 0 ~ "gray70",
        TRUE ~ "gray30"
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
      label = format(round(abs(Value), 0), nsmall = 0),
      label_x = if_else(Value >= 0, end + 5, end - 5),
      hjust = if_else(Value >= 0, 0, 1)
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

  df_subtotals <- df %>% filter(type == "subtotal")

  # Apply a function over a list of integers from 1 to count of sub total rows
  df_sub_connectors <- bind_rows(lapply(seq_len(nrow(df_subtotals)), function(i) {
    # Get the current row
    subtotal_row <- df_subtotals[i, ]
    # Get the current group id
    group_id <- subtotal_row$subtotal_group
    # Get the bar rows from df whose group id matches the current subtotal row
    bar_rows <- df %>% filter(type == "bar", subtotal_group == group_id)

    # If there are no such bars, then this subtotal is invalid (user incorrectly defined)
    if (nrow(bar_rows) < 1) return(NULL)

    # There are two lines which visually connect a subtotal with its components:
    # 1) A line from the bottom (i.e. left) of the first bar in the group
    # to the bottom (i.e. left) of the sub-total bar
    # 2) A line from the top (i.e. right) of the last bar in the group
    # to the top (i.e. right) of the sub-total bar
    first_bar <- bar_rows[1, ]
    last_bar <- bar_rows[nrow(bar_rows), ]

    # Each sub-total produces two rows representing the lines connecting the
    # bars with their associated sub-total bar
    tibble(
      x = c(first_bar$start, last_bar$end),
      xend = c(first_bar$start, last_bar$end),
      y = c(first_bar$x - 0.3, last_bar$x - 0.3),
      yend = c(subtotal_row$x + 0.3, subtotal_row$x + 0.3)
    )
  }))

  df_results <- df %>% filter(type == "result")

  # The logic here follows similar logic to the sub-total bars described above
  # the slight difference is that since a result is always from zero
  # only one connecting line is needed - it connects the top of the previous bar
  # (subtotal or otherwise) with the current result bar
  df_result_connectors <- bind_rows(lapply(seq_len(nrow(df_results)), function(i) {
    result_row <- df_results[i, ]
    if (result_row$x == 1) return(NULL)

    prev_row <- df %>% filter(x == result_row$x - 1)
    if (nrow(prev_row) == 0) return(NULL)

    tibble(
      x = prev_row$end,
      xend = result_row$end,
      y = prev_row$x - 0.3,
      yend = result_row$x + 0.3
    )
  }))

  # This places all the coordinates of the three types of connecting lines
  # into a single tibble so they can all be drawn at once.
  df_connectors <- bind_rows(df_lines, df_sub_connectors, df_result_connectors)

  ggplot(df %>% filter(type != "divider")) +
    # draw the bars
    geom_rect(aes(ymin = x - 0.3, ymax = x + 0.3, xmin = start, xmax = end, fill = fill),
              color = NA) +
    # draw the connecting lines
    geom_segment(
      data = df_connectors,
      aes(x = x, xend = xend, y = y, yend = yend),
      color = "gray70",
      linewidth = 0.2,
      inherit.aes = FALSE
    ) +
    # draw the bar labels
    # geom_segment(data = df_labels,
    #              aes(x = label_x, y = x, xend = label_x, yend = x),
    #              color = "red", linewidth = 0.2) +
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
      plot.title.position = 'panel'
    )
}

