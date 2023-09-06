#' Module UI

#' @title mod_estimate_ui and mod_estimate_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @keywords internal

mod_estimate_ui <- function(id, thisgroup) {
	ns <- NS(id)
	tabItem(
		tabName = "estimate",
		fluidRow(
		  column(
		    width = 4,

		    sliderInput( inputId =   ns("e"),
		                 label = "Tolerable margin of error ",
		                 value = 5,  min = 1 , max = 10, step = 1 ,
		                 width = '100%')
		  ),
		  column(
		    width = 4,
		    sliderInput( inputId =   ns("ci"),
		                 label = "Confidence level",
		                 value = 95,  min = 80 , max = 99, step = 1 ,
		                 width = '100%')
		  ),
		  column(
		    width = 4,
		    sliderInput( inputId =   ns("p"),
		                 label = "Anticipated response distribution",
		                 value = 0.5,  min = 0 , max = 1, step = 0.05 ,
		                 width = '100%')
		  )
		),
		fluidRow(
		  column(
		    width = 6,
		    sliderInput( inputId =   ns("over"),
		                 label = "Desired oversampling proportion",
		                 value = 0.2,  min = 0 , max = 1, step = 0.1 ,
		                 width = '100%')
		  ),
		  column(
		    width = 6,
		    sliderInput( inputId =   ns("power"),
		                 label = "Desired Sample Power",
		                 value = 0.2,  min = 0 , max = 1, step = 0.1 ,
		                 width = '100%'),
		  )
		),
		fluidRow(
		  column(
		    width = 12,
		    "Illustration "
		  )

		)
	)
}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @keywords internal

mod_estimate_server <- function(input, output, session, AppReactiveValue, thisgroup) {
	ns <- session$ns
}

## copy to body.R
# mod_estimate_ui("estimate_ui_1")

## copy to sidebar.R
# shinydashboard::menuItem("displayName",tabName = "estimate",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_estimate_server, "estimate_ui_1", AppReactiveValue)

