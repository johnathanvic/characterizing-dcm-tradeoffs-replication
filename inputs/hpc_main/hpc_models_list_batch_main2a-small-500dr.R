all_model_specs <- list(
  # # Basic Models
  # list(name = "MNL_none_pref",           heterogeneity_spec = "none"),
  # list(name = "NL_NL_pref",            heterogeneity_spec = "none",   nesting_spec = "NL-THREE"),
  # list(name = "CNL_CNL_pref",           heterogeneity_spec = "none",   nesting_spec = "CNL"),
  # 
  # # Latent Class Models
  # list(name = "LCL2_none_pref",          heterogeneity_spec = "latent", n_classes = 2),
  # list(name = "LCL2_NL_pref",             heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 2),
  # list(name = "LCL2_CNL_pref",            heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 2),
  # 
  # list(name = "LCL4_none_pref",          heterogeneity_spec = "latent", n_classes = 4),
  # list(name = "LCL4_NL_pref",             heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 4),
  # list(name = "LCL4_CNL_pref",            heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 4),
  # 
  # list(name = "LCL6_none_pref",          heterogeneity_spec = "latent", n_classes = 6),
  # list(name = "LCL6_NL_pref",             heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 6),
  # list(name = "LCL6_CNL_pref",            heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 6),
  # 
  # list(name = "LCL8_none_pref",          heterogeneity_spec = "latent", n_classes = 8),
  #
  # # Mixed Logit - 100 draws
  # list(name = "MXL100_none_pref",        heterogeneity_spec = "mixed",  n_draws = 100),
  # list(name = "MXL100_none_wtp",         heterogeneity_spec = "mixed",  n_draws = 100,  utility_space = "wtp")
  
  # Mixed Logit - 500 draws
  list(name = "MXL500_none_pref",        heterogeneity_spec = "mixed",  n_draws = 500),
  list(name = "MXL500_none_wtp",         heterogeneity_spec = "mixed",  n_draws = 500,  utility_space = "wtp")
  
)
# Big
# all_model_specs <- list(
#   # LCL
#   list(name = "LC8_NL_pref",             heterogeneity_spec = "latent", nesting_spec = "NL-THREE", n_classes = 8),
#   list(name = "LC8_CNL_pref",            heterogeneity_spec = "latent", nesting_spec = "CNL", n_classes = 8),
#   # Mixed Logit - 1k draws
#   list(name = "MXL1k_none_pref",         heterogeneity_spec = "mixed",  n_draws = 1000),
#   list(name = "MXL1k_none_wtp",          heterogeneity_spec = "mixed",  n_draws = 1000, utility_space = "wtp")
# )