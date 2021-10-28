test_that("Default plot_forest returns a ggplot object", {
  plot_forest_res <- plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = max(c(1, mr_res_example$or_uci95)), style = "OR")
  expect_s3_class(plot_forest_res, "ggplot")
  })
