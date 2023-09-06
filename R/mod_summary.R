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
		    p("The way a sample is drawn has major implications for the usability and usefulness of survey data,
and careful sample size calculations can lead to considerably more efficient designs and cost savings
than ad-hoc and poorly thought through designs."),
		    p("However, it is important to note that the sample size is not always the main
driver of cost, and a larger population does not necessarily require a higher sample size."),
		    p("Also recall that a bigger sample size will not improve your estimates accuracy if
		      the random selection of participants is not respected.")
		  )
		),

		fluidRow(
		  shinydashboard::box(
		    title = "Sample Parameters - Precision Goals",
		    #  status = "primary",
		    status = "info",
		    solidHeader = FALSE,
		    collapsible = TRUE,
		    #background = "light-blue",
		    width = 12,
		    fluidRow(
		      tabsetPanel(type = "tabs",
		                  tabPanel(title= "Pillar-1-Refugee",
		                           id = ns("target_ras"),
		                           mod_estimate_ui(ns("estimate_ui_1"), thisgroup = "RAS")),
		                  tabPanel(title= "Pillar-2-Stateless",
		                           id = ns("target_sta"),
		                           mod_estimate_ui(ns("estimate_ui_2"), thisgroup = "STA")),
		                  tabPanel(title= "Pillar-3-Reintegration",
		                           id = ns("target_ret"),
		                           mod_estimate_ui(ns("estimate_ui_3"), thisgroup = "RET")) ,
		                  tabPanel(title= "Pillar-4-IDP",
		                           id = ns("target_idp"),
		                           mod_estimate_ui(ns("estimate_ui_4") , thisgroup = "IDP") ),
		                  tabPanel(title= "Other People with and for whom UNHCR works",
		                           id = ns("target_ooc"),
		                           mod_estimate_ui(ns("estimate_ui_5"), thisgroup = "OOC") )
		      ) ## End Tabset


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


	callModule(mod_estimate_server, "estimate_ui_1", AppReactiveValue, thisgroup = "RAS")
	callModule(mod_estimate_server, "estimate_ui_1", AppReactiveValue, thisgroup = "STA")
	callModule(mod_estimate_server, "estimate_ui_4", AppReactiveValue, thisgroup = "RET")
	callModule(mod_estimate_server, "estimate_ui_2", AppReactiveValue, thisgroup = "IDP")
	callModule(mod_estimate_server, "estimate_ui_3", AppReactiveValue, thisgroup = "OOC")
}

## copy to body.R
# mod_summary_ui("summary_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "summary",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_summary_server, "summary_ui_1", AppReactiveValue)

