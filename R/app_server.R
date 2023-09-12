#' Server
#'
#' This function is internally used to manage the shinyServer
#'
#' @import shiny
#' @import shinydashboard
#' @noRd
#' @keywords internal
app_server <- function(input, output, session) {

  ## add a reactive value object to pass by elements between objects
  AppReactiveValue <-  reactiveValues(
    country = NULL,
    poptype = NULL,
    universe =     ggplot2::ggplot() +
      ggplot2::annotate("text", x = 1, y = 1, size = 11,
                        label = "A sampling universe refers to the total population \nfrom which a sample is drawn for a survey." ) +
      ggplot2::theme_void(),

    ## Sample info for ras
    ras_universe = NULL,
    ras_sampling = NULL,
    ras_availablereg  = NULL,
    ras_lessthan5000  = NULL,
    ras_spread  = NULL,
    ras_strata  = NULL,
    ras_budget  = NULL,
    ras_gather  = NULL,
    ras_expert   = NULL,

    ## Sample info for sta
    sta_universe = NULL,
    sta_sampling = NULL,
    sta_availablereg  = NULL,
    sta_lessthan5000  = NULL,
    sta_spread  = NULL,
    sta_strata  = NULL,
    sta_budget  = NULL,
    sta_gather  = NULL,
    sta_expert   = NULL,

    ## Sample info for ret
    ret_universe = NULL,
    ret_sampling = NULL,
    ret_availablereg  = NULL,
    ret_lessthan5000  = NULL,
    ret_spread  = NULL,
    ret_strata  = NULL,
    ret_budget  = NULL,
    ret_gather  = NULL,
    ret_expert   = NULL,

    ## Sample info for idp
    idp_universe = NULL,
    idp_sampling = NULL,
    idp_availablereg  = NULL,
    idp_lessthan5000  = NULL,
    idp_spread  = NULL,
    idp_strata  = NULL,
    idp_budget  = NULL,
    idp_gather  = NULL,
    idp_expert   = NULL,

    ## Sample info for ooc
    ooc_universe = NULL,
    ooc_sampling = NULL,
    ooc_availablereg  = NULL,
    ooc_lessthan5000  = NULL,
    ooc_spread  = NULL,
    ooc_strata  = NULL,
    ooc_budget  = NULL,
    ooc_gather  = NULL,
    ooc_expert   = NULL
  )

  # pins::board_register() # connect to pin board if needed
  callModule(mod_home_server, "home_ui_1")
  callModule(mod_configure_server, "configure_ui_1", AppReactiveValue)
  callModule(mod_assumptions_server, "assumptions_ui_1", AppReactiveValue)
  callModule(mod_summary_server, "summary_ui_1", AppReactiveValue)
  callModule(mod_report_server, "report_ui_1", AppReactiveValue)
}
