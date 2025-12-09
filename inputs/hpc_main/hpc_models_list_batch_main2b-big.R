all_model_specs <- list(
  # LCL
  list(name = "LCL8_NL_pref",             heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 8),
  list(name = "LCL8_CNL_pref",            heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 8),
  # Mixed Logit - 1k draws
  list(name = "MXL1k_none_pref",         heterogeneity_spec = "mixed",  n_draws = 1000),
  list(name = "MXL1k_none_wtp",          heterogeneity_spec = "mixed",  n_draws = 1000, utility_space = "wtp")
)