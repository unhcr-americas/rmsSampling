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
		  ## Parameter
		  column(
		    width = 6,
		    fluidRow(  ## first line
      		  column(
      		    width = 4,
      		    sliderInput( inputId =   ns("ci"),
      		                 label = "Confidence Level",
      		                 value = 95,  min = 80 , max = 99, step = 1 ,
      		                 width = '100%'),
      		    p("A higher confidence level will result
      		      in a wider confidence interval and a larger margin of error.
      		      This means you are more certain that the true unemployment rate
      		      falls within the wider range provided. However, it also means
      		      you are less precise in estimating the rate.",
      		      style = "font-size: 11px")
      		  ),
    		    column(
    		      width = 4,
    		      sliderInput( inputId =   ns("e"),
    		                   label = "Tolerable Margin of Error ",
    		                   value = 5,  min = 1 , max = 10, step = 1 ,
    		                   width = '100%'),
    		      p("The decision you make regarding the balance between the confidence
          		    interval and margin of error involves a trade-off.
          		    A narrower confidence interval with a smaller margin of error
          		    provides a more precise estimate but with lower confidence.
          		    A wider confidence interval with a larger margin of error provides
          		    higher confidence but a less precise estimate.",
    		        style = "font-size: 11px")
    		    ),
      		  column(
      		    width = 4,
      		    sliderInput( inputId =   ns("p"),
      		                 label = "Anticipated response distribution",
      		                 value = 0.5,  min = 0 , max = 1, step = 0.05 ,
      		                 width = '100%'),
      		    p("The variability of your main point of study within your population
      		      influences the confidence interval. If there is high variability,
      		      your confidence interval will be wider. This is because it's more
      		      challenging to estimate a single average when the data points
      		      are spread out over a wide range.",
      		      style = "font-size: 11px")
      		  )
		),  ## end first line
		fluidRow( ## second line
		  column(
		    width = 6,
		    sliderInput( inputId =   ns("power"),
		                 label = "Desired Sample Power",
		                 value = 0.2,  min = 0 , max = 1, step = 0.1 ,
		                 width = '100%'),
		    p("Statistical power is a measure of the likelihood of detecting a true effect
		    or difference when it exists for instance between two yearly iteration of your survey.
		    Increasing the sample size generally improves statistical power: If you want to be
		      able to change a change of 5% between 2 differents years, you will need to
		      increase the sample size accordingly.",
		      style = "font-size: 11px")
		  ),
		  column(
		    width = 6,
		    sliderInput( inputId =   ns("over"),
		                 label = "Desired oversampling proportion",
		                 value = 0.2,  min = 0 , max = 1, step = 0.1 ,
		                 width = '100%'),
		    p("Oversampling in a household survey is necessary to ensure adequate
		      representation of underrepresented or rare populations, enhancing the
		      accuracy and precision of estimates for these groups. This approach
		      allows for more reliable analysis and equitable research outcomes.
		      However, it may increase survey costs and complexity, requiring careful
		      weighting and analysis to address potential biases..",
		      style = "font-size: 11px")
		  )
	  	)  ## End first line
		),  ## end first column
 	  column(  ## Column to show visually the impact
		    width = 6,

		    p("For instance, you are aiming to measure outcome Outcome 13.3/ SDG 8.5.2
		      (i.e. Proportion of Persons of Concern (working age) who are unemployed).
		      You are able to use a probabilistic sampling approach for your survey, let's the impact
		      of the parameters on the final results!",
		      style = "font-size: 11px"),
		    "Box with metric sample size ",
		    hr(),
		    "Illustration on chart with bell curve ",
		    hr(),
		    "Illustration with bar chart with error "
		  )
  	) ## End box
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

