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
		    title = "Sampling Parameters - Precision Goals in case a probabilistic approach can be used...",
		    #  status = "primary",
		    status = "info",
		    solidHeader = FALSE,
		    collapsible = TRUE,
		    #background = "light-blue",
		    width = 12,
		    fluidRow(
		      radioButtons(  inputId = ns("poptype3"),
		                     label = " Set up for each of the group you need to sample ",
		                     inline = TRUE ,
		                     choices = c(
		                       "Pillar-1- Refugee, Asylum Seeker & Other in Need of International Protection" ="RAS",
		                       "Pillar-2- Stateless"="STA",
		                       "Pillar-3- Returnees"= "RET",
		                       "Pillar-4- Internally Displaced Persons"=  "IDP",
		                       "Other People with and for whom UNHCR works"= "OOC" )
		      ),
		      tabsetPanel(
		        id = ns("hidden_tabs2"),
		        type = "hidden",
		        tabPanel(title= "target_RAS",
		                 mod_estimate_ui(ns("estimate_ui_1"), thisgroup = "RAS")),
		        tabPanel(title= "target_STA",
		                 mod_estimate_ui(ns("estimate_ui_2"), thisgroup = "STA")),
		        tabPanel(title= "target_RET",
		                 mod_estimate_ui(ns("estimate_ui_3"), thisgroup = "RET") ),
		        tabPanel(title= "target_IDP",
		                 mod_estimate_ui(ns("estimate_ui_4") , thisgroup = "IDP") ),
		        tabPanel(title= "target_OOC",
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

	observe({
	  ## Update filters for the next steps..
	  req(AppReactiveValue$poptypefilt)
	  updateRadioButtons(session,
	                     "poptype3",
	                     choices = AppReactiveValue$poptypefilt |> dplyr::pull(pop) |>
	                       purrr::set_names(AppReactiveValue$poptypefilt |> dplyr::pull(lab))
	  )
	})


	## Display conditionally....
	observeEvent(eventExpr = input$poptype3, {
	  req(input$poptype3)
	  print(paste0("Selected  : ", input$poptype3))
	  updateTabsetPanel(session,
	                    "hidden_tabs2",
	                    selected = paste0("target_", input$poptype3))
	})
}

## copy to body.R
# mod_summary_ui("summary_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "summary",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_summary_server, "summary_ui_1", AppReactiveValue)

