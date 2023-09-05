#' Module UI

#' @title mod_configure_ui and mod_configure_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @keywords internal

mod_configure_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "configure",

		fluidRow(
		  column(
		    width = 12,
		    h2('Configure the sampling assistant!'),
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
		    title = "Operation Context",
		    #  status = "primary",
		    status = "info",
		    solidHeader = FALSE,
		    collapsible = TRUE,
		    #background = "light-blue",
		    width = 12,
		    fluidRow(

		      selectizeInput(inputId = ns("country"),
		                     label = " Select Country",
		                     choices = ForcedDisplacementStat::end_year_population_totals |>
		                       dplyr::arrange(CountryAsylumName) |>
		                       dplyr::select(CountryAsylumCode) |>
		                       dplyr::distinct()  |>
		                       dplyr::pull(CountryAsylumCode) |>
		                       purrr::set_names(
		                         ForcedDisplacementStat::end_year_population_totals |>
		                           dplyr::arrange(CountryAsylumName) |>
		                           dplyr::select(CountryAsylumName) |>
		                           dplyr::distinct()|>
		                           dplyr::pull(CountryAsylumName) ),
		                     selected = "PAN",
		                     multiple = FALSE,
		                     options = NULL),

		      checkboxGroupInput(  inputId = ns("pop_type"),
		                           label = "Population Types to include for the RMS
		                           based on your Means of Verification (MoV) table
		                           and secondary data review",
		                           choices = c("Refugee & Asylum Seeker (includes Other in Need of International Protection" ="RAS",
		                                       "Other of Concern"= "OOC",
		                                       "Stateless"="STA",
		                                       "Internally Displaced Persons"=     "IDP"  ,
		                                       "Returnees"= "RET"),
		                           selected = c("RAS",  "OOC",  "STA",  "IDP" )
		      )
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

mod_configure_server <- function(input, output, session, AppReactiveValue) {
	ns <- session$ns


	observe({
	  AppReactiveValue$country <- input$country
	})

}

## copy to body.R
# mod_configure_ui("configure_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "configure",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_configure_server, "configure_ui_1", AppReactiveValue)

