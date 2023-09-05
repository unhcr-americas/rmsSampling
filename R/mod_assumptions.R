#' Module UI

#' @title mod_assumptions_ui and mod_assumptions_server
#' @description A shiny module with key quetions extracted from
#'  https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/common-good-data-initiatives/rms/toolbox-subpage/2-design-and-collect/RMS%20sampling%20decision%20tree.pdf
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @keywords internal

mod_assumptions_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "assumptions",
		fluidRow(
		  column(
		    width = 12,
		    h2('Decision Tree'),
		    p("The RMS sampling decision tree serves as a methodological framework for
RMS quality assurance as well as being a tool to decide on most appropriate methodological approaches."),
		    p("Answer the questions in each box to determine which sampling method is
		    most appropriate for your RMS based on the country context for each population group included"),
		    p(" For a broader overview of non probabilistic sampling approaches, you may consult: ",
		      tags$a(href="https://comparativemigrationstudies.springeropen.com/articles/10.1186/s40878-016-0044-9/tables/1",
		             "Ref1"),
		      ", ",
		      tags$a(href="https://www.questionpro.com/blog/non-probability-sampling",
		             "Ref2"),
		      " or ",
		      tags$a(href="https://www150.statcan.gc.ca/n1/edu/power-pouvoir/ch13/nonprob/5214898-eng.htm",
		             "Ref2"),
		      ". ")
		  )
		),

		tabsetPanel(type = "tabs",
		            tabPanel(title= "Refugees & Asylum Seeker",
		                     mod_screener_ui(ns("screener_ui_1") )),

		            tabPanel(title= "IDPS",
		                     mod_screener_ui(ns("screener_ui_2") ) ),

		            tabPanel(title= "Other of Concerns",
		                     mod_screener_ui(ns("screener_ui_3") ) ) ,

		            tabPanel(title= "Returnees",
		                     mod_screener_ui(ns("screener_ui_4") ) )
		) ## End Tabset


	)
}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @keywords internal

mod_assumptions_server <- function(input, output, session, AppReactiveValue) {
	ns <- session$ns


	callModule(mod_screener_server, "screener_ui_1", AppReactiveValue, thisgroup = "RAS")
	callModule(mod_screener_server, "screener_ui_2", AppReactiveValue, thisgroup = "IDP")
	callModule(mod_screener_server, "screener_ui_3", AppReactiveValue, thisgroup = "OOC")
	callModule(mod_screener_server, "screener_ui_4", AppReactiveValue, thisgroup = "RET")

	##




}

## copy to body.R
# mod_assumptions_ui("assumptions_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "assumptions",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_assumptions_server, "assumptions_ui_1", AppReactiveValue)

