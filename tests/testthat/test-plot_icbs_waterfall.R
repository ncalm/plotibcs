test_that("it returns a ggplot", {
  my_data <- tibble::tibble(
    position = 1:6,
    type = c("bar", "bar", "bar", "divider", "subtotal", "result"),
    bold = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
    subtotal_group = c(1, 1, 1, NA, 1, NA),
    `Line.Item` = c("Licences", "Consulting", "Maintenance", NA, "Sales revenue", "Result"),
    PY = c(700, 200, 100, NA, NA, NA)
  )

  result <- plotibcs::plot_ibcs_waterfall(my_data, "Test", "PY", "Line.Item")
  expect_s3_class(result, "ggplot")
})
