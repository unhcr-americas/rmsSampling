#' UI Side menau
#'
#' This function is internally used to manage the side menu
#'
#' @import shiny
#' @import shinydashboard
#' @noRd
#' @keywords internal
#'
sidebar <- function() {
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      ## Here the menu item entry to the first module
      shinydashboard::menuItem("About",tabName = "home",icon = icon("bookmark")),
      shinydashboard::menuItem("Configure Country",tabName = "configure",icon = icon("globe")),
      shinydashboard::menuItem("Set Assumptions",tabName = "assumptions",icon = icon("gears")),
      shinydashboard::menuItem("Estimate Size",tabName = "summary",icon = icon("chalkboard-user"))

      # - add more - separated by a comma!
      ## For icon search on https://fontawesome.com/search?o=r&m=free - filter on free
    )
  )
}
