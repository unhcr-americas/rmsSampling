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
	golem::activate_js()
	tabItem(
		tabName = "assumptions",
		fluidRow(
		  column(
		    width = 12,
		    h2('Decision Tree'),
		    p("The RMS sampling decision tree serves as a methodological framework for
RMS quality assurance as well as being a tool to decide on most appropriate methodological approaches."),
		    p("Answer the questions below to determine which sampling method is
		    most appropriate for your RMS based on the country context for each population group included")#,
		    # p(" For a broader overview of non probabilistic sampling approaches, you may consult: ",
		    #   tags$a(href="https://comparativemigrationstudies.springeropen.com/articles/10.1186/s40878-016-0044-9/tables/1",
		    #          "Ref1"),
		    #   ", ",
		    #   tags$a(href="https://www.questionpro.com/blog/non-probability-sampling",
		    #          "Ref2"),
		    #   " or ",
		    #   tags$a(href="https://www150.statcan.gc.ca/n1/edu/power-pouvoir/ch13/nonprob/5214898-eng.htm",
		    #          "Ref2"),
		    #  ". ")
		  )
		),

		fluidRow(
		  shinydashboard::box(
		    title = paste0("Sampling Assessment Questions") ,
		    #  status = "primary",
		    status = "info",
		    solidHeader = FALSE,
		    collapsible = TRUE,
		    #background = "light-blue",
		    width = 12,
		    radioButtons(  inputId = ns("poptype2"),
		                                label = " Document the screening below for each of the group you need to sample ",
		                                inline = TRUE ,
		                                #character(0)
		                                choices = c(
		                                  "Pillar-1- Refugee, Asylum Seeker & Other in Need of International Protection" ="RAS",
		                                  "Pillar-2- Stateless"="STA",
		                                  "Pillar-3- Returnees"= "RET",
		                                  "Pillar-4- Internally Displaced Persons"=  "IDP",
		                                  "Other People with and for whom UNHCR works"= "OOC" )
		    ),
	    	tabsetPanel(
	    	  id = ns("hidden_tabs"),
	    	  type = "hidden",
  		            tabPanel(title= "target_RAS",
  		                     mod_screener_ui(ns("screener_ui_1"), thisgroup = "RAS")),
  		            tabPanel(title= "target_STA",
		                     mod_screener_ui(ns("screener_ui_2"), thisgroup = "STA")),
  		            tabPanel(title= "target_RET",
  		                     mod_screener_ui(ns("screener_ui_3"), thisgroup = "RET") ),
  		            tabPanel(title= "target_IDP",
  		                     mod_screener_ui(ns("screener_ui_4") , thisgroup = "IDP") ),
		              tabPanel(title= "target_OOC",
		                     mod_screener_ui(ns("screener_ui_5"), thisgroup = "OOC") )
		) ## End Tabset
		) # end box
	) # end fluid row

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
	callModule(mod_screener_server, "screener_ui_2", AppReactiveValue, thisgroup = "STA")
	callModule(mod_screener_server, "screener_ui_3", AppReactiveValue, thisgroup = "RET")
	callModule(mod_screener_server, "screener_ui_4", AppReactiveValue, thisgroup = "IDP")
	callModule(mod_screener_server, "screener_ui_5", AppReactiveValue, thisgroup = "OOC")


	 observe({
	#   ## Update filters for the next steps..
    req(AppReactiveValue$poptypefilt)
	  updateRadioButtons(session,
	                     "poptype2",
	                     choices = AppReactiveValue$poptypefilt |> dplyr::pull(pop) |>
	                       purrr::set_names(AppReactiveValue$poptypefilt |> dplyr::pull(lab))
	  )
	 })



	## Display conditionally....
	observeEvent(eventExpr = input$poptype2, {

	  req(input$poptype2)
	  print(paste0("Selected  : ", input$poptype2))
	  updateTabsetPanel(session,
	                    "hidden_tabs",
	                    selected = paste0("target_", input$poptype2))
	})

	 # observe({
	    # We'll use the input$controller variable multiple times, so save it as x
	    # for convenience.
	    # x <- input$controller
	    #
	    # r_options <- list()
	    # r_options[[sprintf("option label %d 1", x)]] <- sprintf("option-%d-1", x)
	    # r_options[[sprintf("option label %d 2", x)]] <- sprintf("option-%d-2", x)
	    #
	    # # Change values for input$inRadio
	    # updateRadioButtons(session, "poptype2", choices = r_options)

	    # # Can also set the label and select an item
	    # updateRadioButtons(session, "inRadio2",
	    #   label = paste("Radio label", x),
	    #   choices = r_options,
	    #   selected = sprintf("option-%d-2", x)
	    # )
	  #})


}

## copy to body.R
# mod_assumptions_ui("assumptions_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "assumptions",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_assumptions_server, "assumptions_ui_1", AppReactiveValue)

