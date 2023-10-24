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
		    tags$span("(Beta)", style = "font-size: 25px")
		  ),
		  br(),
		  ### Then a short explainer
		  p( "This ",tags$span("companion app", style = "color:#00B398"), " complements Result Monitoring Survey (RMS)  technical guidances: ",
		     tags$a(href="https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/common-good-data-initiatives/rms/main-page/RMS%20design%20and%20implementation%20management%20Guidance.pdf",
		            "Design and Implementation Management Guidance"),", ",
		     tags$a(href="https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/common-good-data-initiatives/rms/toolbox-subpage/2-design-and-collect/RMS%20basic%20sampling%20guidance.pdf",
		            "Sampling Guidance"),", ",
		     tags$a(href="https://intranet.unhcr.org/content/dam/unhcr/intranet/staff%20support/common-good-data-initiatives/rms/toolbox-subpage/2-design-and-collect/RMS%20sampling%20decision%20tree.pdf",
		           "Sampling Decision Tree"),". The app guides users throught the different decision points to address during the preparation phase,
		     including study design, adequate sampling technique, expected results disaggregation and indicators precision goals. ",
		    style = "font-size: 16px"),
		  br(),
		  p( "Building a sampling plan is not a mechanical process and needs back and forth review in order to be optimal. Using information already available from UNHCR Official Statistics for each country,
		     and based on the confirmation of sampling assumptions, this ",tags$span("companion app", style = "color:#00B398"), " will help operations building an initial
		      ",
		     strong("Methodological Approach Documentation "),
		            "that can then be reviewed by regional DIMA in order to inform the preparation of an RMS survey.",
		     style = "font-size: 16px"),
		  br(),
		  p("This app is part of a ",tags$span("comprehensive app toolkit", style = "color:#00B398"),
		    " to mainstream knowledge & enhance the process of survey implementation through Kobotoolbox. It includes: ",
		    tags$a(href="https://rstudio.unhcr.org/rmsSampling/", "rmsSampling"), " to help designing sampling strategies, ",
		    tags$a(href="https://rstudio.unhcr.org/Survey_Designer", "SurveyDesigner"), " to help integrating annual survey needs, ",
		    tags$a(href="https://rstudio.unhcr.org/XlsFormUtil/", "XlsFormUtil"), " to help reviewing form contextualisation, ",
		    tags$a(href="https://rstudio.unhcr.org/HighFrequencyChecks/", "HighFrequencyChecks"), " to monitor data collection quality, ",
		    tags$a(href="https://rstudio.unhcr.org/kobocruncher/", "KoboCruncher"), " to perform rapid data exploration and compile indicators.",
		    style = "font-size: 12px"),
		  p(tags$i( class = "fa fa-github"),
		    "App built with ",
		    tags$a(href="https://edouard-legoupil.github.io/graveler/",
		           "{graveler}" ),
		    " -- report ",
		    tags$a(href="https://github.com/unhcr-americas/rmsSampling/issues/new/choose",
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

