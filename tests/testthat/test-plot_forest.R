test_that("Default plot_forest returns a ggplot object", {
  plot_forest_res <- plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = max(c(1, mr_res_example$or_uci95)), style = "OR")
  expect_s3_class(plot_forest_res, "ggplot")
  plot_forest_res <- plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = b, effect_signif = 3, ci_lower = lo_ci, ci_upper = up_ci, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(0, mr_res_example$lo_ci)), xmax = max(c(0, mr_res_example$up_ci)), style = "Beta")
  expect_s3_class(plot_forest_res, "ggplot")
  })

test_that("Invalid input parameters returns an error", {
  expect_error(plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = max(c(1, mr_res_example$or_uci95)), style = "x"))
  expect_error(plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = "3", ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = max(c(1, mr_res_example$or_uci95)), style = "OR"))
  expect_error(plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = "3", p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = max(c(1, mr_res_example$or_uci95)), style = "OR"))
  expect_error(plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = "min(c(1, mr_res_example$or_lci95))", xmax = max(c(1, mr_res_example$or_uci95)), style = "OR"))
  expect_error(plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = "max(c(1, mr_res_example$or_uci95))", style = "OR"))
  expect_error(plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = max(c(1, mr_res_example$or_uci95)), font_size = "12", style = "OR"))
  expect_error(plot_forest(mr_res_example, cols_left = vars("Protein" = exposure), effect = or, effect_signif = 3, ci_lower = or_lci95, ci_upper = or_uci95, ci_signif = 3, p_value = pval, width_ratio = c(0.5, 3, 1.5), xmin = min(c(1, mr_res_example$or_lci95)), xmax = max(c(1, mr_res_example$or_uci95)), hline = "0", style = "OR"))
  })
