#' Module UI

#' @title mod_report_ui and mod_report_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @keywords internal

mod_report_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "report",
		fluidRow(
		  fluidRow(
		    column(
		      width = 12,
		      h2('Sampling Design Documentation'),
		      p("As you have finalised all the steps before, you can now download the
		        documentation to include as an annex of your survey TOR.
		        You may also upload as a ressource witin the RILD dataset space"),
		      p("In case, you have an enumeration list, do not hesitate to reach out
		        to your regional DIMA to obtain support for the sample drawing script.")
		    )
		  ),

		  fluidRow(
		    shinydashboard::box(
		      title = "Report",
		      #  status = "primary",
		      status = "info",
		      solidHeader = FALSE,
		      collapsible = TRUE,
		      #background = "light-blue",
		      width = 12,
		      actionButton(inputId = ns("showreport"),
		                   label = "Donwload Your report"),
		      hr()
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

mod_report_server <- function(input, output, session, AppReactiveValue) {
	ns <- session$ns
}

## copy to body.R
# mod_report_ui("report_ui_1")

## copy to sidebar.R
# shinydashboard::menuItem("displayName",tabName = "report",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_report_server, "report_ui_1", AppReactiveValue)

