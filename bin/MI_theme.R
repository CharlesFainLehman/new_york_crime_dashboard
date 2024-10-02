library(bslib)

MI_theme <- bs_theme(bg = '#f0f3ff', fg = '#20222e', base_font = "Le Monde Livre") %>%
  bs_add_rules(
    "
    .bslib-navbar .nav-link, .bslib-navbar .navbar-brand {
        padding-top: 0 !important;
        padding-bottom: 0 !important;  /* Adjusts padding for the navbar */
    }
    .navbar {
        margin-bottom: 5px !important;
        margin-top: 1px !important;
        /* Reduce space below the navbar */
    }
    
    .sliderInput .irs-bar { background-color: #FF6347; }
    .sliderInput .irs-bar-edge { background-color: #FF6347; }
    .sliderInput .irs-single { background-color: #FF6347; }
    "
  )