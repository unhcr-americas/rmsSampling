#' Module UI

#' @title mod_summary_ui and mod_summary_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @keywords internal

mod_summary_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "summary",

		fluidRow(
		  column(
		    width = 12,
		    h2('Estimate Size'),
		    p("Sampling describes the selection of a subset of individuals or households from a population of
		interest in a controlled way that allows us to make generalisations about the characteristics of that
		population without interviewing everyone."),
		    p("Before embarking on your Result Monitoring Survey, it is therefore essential
		to have a complete and sound Means of Verification (MoV) table that clearly
		defines not only the indicators, but also the popuation group for which survey-based data is required.")
		  )
		),

		fluidRow(
		  shinydashboard::box(
		    title = "Sample Parameters",
		    #  status = "primary",
		    status = "info",
		    solidHeader = FALSE,
		    collapsible = TRUE,
		    #background = "light-blue",
		    width = 12,
		    fluidRow(
		    )
		  )
		)
	)
}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @keywords internal

mod_summary_server <- function(input, output, session, AppReactiveValue) {
	ns <- session$ns
}

## copy to body.R
# mod_summary_ui("summary_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "summary",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_summary_server, "summary_ui_1", AppReactiveValue)

