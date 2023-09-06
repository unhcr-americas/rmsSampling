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
	    	tabsetPanel(type = "tabs",
  		            tabPanel(title= "Pillar-1-Refugee",
  		                     id = ns("target_ras"),
  		                     mod_screener_ui(ns("screener_ui_1"), thisgroup = "RAS")),
  		            tabPanel(title= "Pillar-2-Stateless",
  		                     id = ns("target_sta"),
		                     mod_screener_ui(ns("screener_ui_2"), thisgroup = "STA")),
  		            tabPanel(title= "Pillar-3-Reintegration",
  		                     id = ns("target_ret"),
  		                     mod_screener_ui(ns("screener_ui_3"), thisgroup = "RET") ),
  		            tabPanel(title= "Pillar-4-IDP",
  		                     id = ns("target_idp"),
  		                     mod_screener_ui(ns("screener_ui_4") , thisgroup = "IDP") ),
		              tabPanel(title= "Other People with and for whom UNHCR works",
		                       id = ns("target_ooc"),
		                     mod_screener_ui(ns("screener_ui_5"), thisgroup = "OOC") )
		) ## End Tabset
		)
	)

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

	## Display conditionally....
	observeEvent(AppReactiveValue$show_ras, {
	  if(isTRUE(AppReactiveValue$show_ras)) {
	    showTab(inputId = "tabs", target = "Pillar-1-Refugee")
	 #  golem::invoke_js("show", paste0("#", ns("target_ras")))
	    # golem::invoke_js("show", paste0("#", ns("screener_ui_1")))
	  } else {
	    hideTab(inputId = "tabs", target = "Pillar-1-Refugee")
	 # golem::invoke_js("hide", paste0("#", ns("target_ras")))
	    #  golem::invoke_js("hide", paste0("#", ns("screener_ui_1")))
	  }
	})

	observeEvent(AppReactiveValue$show_sta, {
	  if(isTRUE(AppReactiveValue$show_sta)) {
	    showTab(inputId = "tabs", target = "Pillar-2-Stateless")
	    #  golem::invoke_js("show", paste0("#", ns("target_sta")))
	   # golem::invoke_js("show", paste0("#", ns("screener_ui_2")))
	  } else {
	    hideTab(inputId = "tabs", target = "Pillar-2-Stateless")
	    #   golem::invoke_js("hide", paste0("#", ns("target_sta")))
	    # golem::invoke_js("hide", paste0("#", ns("screener_ui_2")))
	  }
	})


	observeEvent(AppReactiveValue$show_ret, {
	  if(isTRUE(AppReactiveValue$show_ret)) {
	    showTab(inputId = "tabs", target = "Pillar-3-Reintegration")
	    #  golem::invoke_js("show", paste0("#", ns("target_ret")))
	    # golem::invoke_js("show", paste0("#", ns("screener_ui_3")))
	  } else {
	    hideTab(inputId = "tabs", target = "Pillar-3-Reintegration")
	    #   golem::invoke_js("show", paste0("#", ns("target_ret")))
	     #golem::invoke_js("show", paste0("#", ns("screener_ui_3")))
	  }
	})


	observeEvent(AppReactiveValue$show_idp, {
	  if(isTRUE(AppReactiveValue$show_idp)) {
	    showTab(inputId = "tabs", target = "Pillar-4-IDP")
	    #  golem::invoke_js("show", paste0("#", ns("target_idp")))
	    #   golem::invoke_js("show", paste0("#", ns("screener_ui_4")))
	  } else {
	    hideTab(inputId = "tabs", target = "Pillar-4-IDP")
	    #  golem::invoke_js("hide", paste0("#", ns("target_idp")))
	   #  golem::invoke_js("hide", paste0("#", ns("screener_ui_4")))
	  }
	})

	observeEvent(AppReactiveValue$show_ooc, {
	  if(isTRUE(AppReactiveValue$show_ooc)) {
	    showTab(inputId = "tabs", target = "Other People with and for whom UNHCR works")
	    #  golem::invoke_js("show", paste0("#", ns("target_ooc")))
	    # golem::invoke_js("show", paste0("#", ns("screener_ui_5")))
	  } else {
	    hideTab(inputId = "tabs", target = "Other People with and for whom UNHCR works")
	    #  golem::invoke_js("hide", paste0("#", ns("target_ooc")))
	    # golem::invoke_js("hide", paste0("#", ns("screener_ui_5")))
	  }
	})

	##




}

## copy to body.R
# mod_assumptions_ui("assumptions_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "assumptions",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_assumptions_server, "assumptions_ui_1", AppReactiveValue)

