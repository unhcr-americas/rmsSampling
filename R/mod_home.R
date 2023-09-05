# Module UI Home

#' @title mod_home_ui and mod_home_server
#' @description A shiny module.
#' @import shiny
#' @import shinydashboard
#' @noRd
#' @keywords internal

mod_home_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "home",
		absolutePanel(  ## refers to a css class
		  id = "splash_panel", top = 0, left = 0, right = 0, bottom = 0,
		  ### Get the name for your tool
		  p(
		    tags$span("RMS ", style = "font-size: 60px"),
		    tags$span("Sampling", style = "font-size: 45px"),
		    tags$span("(-> in dev -not yet working)", style = "font-size: 25px")
		  ),
		  br(),
		  ### Then a short explainer
		  p( "The Result Monitoring Survey (RMS) sampling App complements RMS technical guidances: ",
		     tags$a(href="https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/common-good-data-initiatives/rms/main-page/RMS%20design%20and%20implementation%20management%20Guidance.pdf",
		            "Design and Implementation Management Guidance"),
		            ", ",
		     tags$a(href="https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/common-good-data-initiatives/rms/toolbox-subpage/2-design-and-collect/RMS%20basic%20sampling%20guidance.pdf",
		            "Sampling Guidance"),
		     ", ",
		     tags$a(href="https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/common-good-data-initiatives/rms/toolbox-subpage/2-design-and-collect/RMS%20sampling%20decision%20tree.pdf",
		           "Sampling Decision Tree"),
		     ". Sample size for a household survey depends on several factors,
		     including the study design, the  required results disaggregation,
		     the expected precision goals and the sampling technique. ",
		    style = "font-size: 20px"),
		  br(),
		  p( "Using information already available from Official Statistics for each country,
		     based on the confirmation of sampling assumptions, this app will
		     provide estimates of required sample size and generate a summary report
		     to inform the preparation of the survey",
		     style = "font-size: 16px"),
		  p(tags$i( class = "fa fa-github"),
		    "App built with ",
		    tags$a(href="https://edouard-legoupil.github.io/graveler/",
		           "{graveler}" ),
		    " -- report ",
		    tags$a(href="https://github.com/unhcr-americas/rmsSampling/issues",
		           "issues here." ,
		    ),
		    style = "font-size: 10px")
		)
	)
}

# Module Server
#' @import shiny
#' @import shinydashboard
#' @noRd
#' @keywords internal

mod_home_server <- function(input, output, session) {
	ns <- session$ns
	# This create the links for the button that allow to go to the next module
	observeEvent(input$go_to_firstmod, {
	  shinydashboard::updateTabItems(
	    session = parent_session,
	    inputId = "tab_selected",
	    selected = "firstmod"
	  )
	})
}

## copy to body.R
# mod_home_ui("home_ui_1")

## copy to app_server.R
# callModule(mod_home_server, "home_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "home",icon = icon("user"))

