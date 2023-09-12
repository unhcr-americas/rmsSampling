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

mod_estimate_ui <- function(id, thisgroup2) {
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
      		    selectInput( inputId =   ns("ci"),
      		                 label = "Confidence Level",
      		                choices =  c( "80%" = "0.8",
      		                             "85%" = "0.85",
      		                             "90%" = "0.90",
      		                             "95%"= "0.95",
      		                             "99%" = "0.99"  ) ,
      		                selected = "0.95",


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
    		      sliderInput( inputId =   ns("moe"),
    		                   label = "Tolerable Margin of Error in % ",
    		                   value = 5,  min = 0 , max = 10, step = 1 ,
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

		    p("Example on dummy data to measure outcome Outcome 13.3/ SDG 8.5.2:
		       Proportion of working age Persons of who are unemployed).
		      Let's visualise the impact of the parameters on the final results!",
		      style = "font-size: 12px"),
		    hr(),
		    plotOutput(outputId = ns("uncertainity"),
		               width = "100%",
		               height = "400px")
		  )
  	) ## End box
	)
}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @import ggplot2
#' @importFrom sampler rsampcalc
#' @importFrom  srvyr as_survey_design group_by summarise survey_prop
#' @importFrom ungeviz stat_confidence_density
#' @import unhcrthemes
#' @keywords internal

mod_estimate_server <- function(input, output, session, AppReactiveValue, thisgroup2) {
	ns <- session$ns

	reactLocal <- reactiveValues(	)

  observeEvent(eventExpr = input$ci ,{
    ## Reinject int the app reactive value
    if( thisgroup2 == "RAS") {
      AppReactiveValue$ras_ci <- as.numeric(input$ci) }
    if( thisgroup2 == "STA") {
      AppReactiveValue$sta_ci <- as.numeric(input$ci)}
    if( thisgroup2 == "RET") {
      AppReactiveValue$ret_ci <- as.numeric(input$ci)}
    if( thisgroup2 == "IDP") {
      AppReactiveValue$idp_ci <- as.numeric(input$ci)}
    if( thisgroup2 == "OOC") {
      AppReactiveValue$ooc_ci <- as.numeric(input$ci)}
  })
  observeEvent(eventExpr = input$moe ,{
    ## Reinject int the app reactive value
    if( thisgroup2 == "RAS") {
      AppReactiveValue$ras_moe <- input$moe / 100}
    if( thisgroup2 == "STA") {
      AppReactiveValue$sta_moe <- input$moe/ 100}
    if( thisgroup2 == "RET") {
      AppReactiveValue$ret_moe <- input$moe/ 100}
    if( thisgroup2 == "IDP") {
      AppReactiveValue$idp_moe <- input$moe/ 100}
    if( thisgroup2 == "OOC") {
      AppReactiveValue$ooc_moe <- input$moe/ 100}
  })
  observeEvent(eventExpr = input$p ,{
    ## Reinject int the app reactive value
    if( thisgroup2 == "RAS") {
      AppReactiveValue$ras_p <- input$p}
    if( thisgroup2 == "STA") {
      AppReactiveValue$sta_p <- input$p}
    if( thisgroup2 == "RET") {
      AppReactiveValue$ret_p <- input$p}
    if( thisgroup2 == "IDP") {
      AppReactiveValue$idp_p <- input$p}
    if( thisgroup2 == "OOC") {
      AppReactiveValue$ooc_p <- input$p}
  })
  observeEvent(eventExpr = input$power ,{
    ## Reinject int the app reactive value
    if( thisgroup2 == "RAS") {
      AppReactiveValue$ras_power <- input$power}
    if( thisgroup2 == "STA") {
      AppReactiveValue$sta_power <- input$power}
    if( thisgroup2 == "RET") {
      AppReactiveValue$ret_power <- input$power}
    if( thisgroup2 == "IDP") {
      AppReactiveValue$idp_power <- input$power}
    if( thisgroup2 == "OOC") {
      AppReactiveValue$ooc_power <- input$power}
  })
  observeEvent(eventExpr = input$over ,{
    ## Reinject int the app reactive value
    if( thisgroup2 == "RAS") {
      AppReactiveValue$ras_over <- input$over}
    if( thisgroup2 == "STA") {
      AppReactiveValue$sta_over <- input$over}
    if( thisgroup2 == "RET") {
      AppReactiveValue$ret_over <- input$over}
    if( thisgroup2 == "IDP") {
      AppReactiveValue$idp_over <- input$over}
    if( thisgroup2 == "OOC") {
      AppReactiveValue$ooc_over <- input$over}
  })


  output$uncertainity <- renderPlot({

    if( thisgroup2 == "RAS") {
      reactLocal$N1 <- AppReactiveValue$ras_universe }
    if( thisgroup2 == "STA") {
      reactLocal$N1 <- AppReactiveValue$sta_universe }
    if( thisgroup2 == "RET") {
      reactLocal$N1 <- AppReactiveValue$ret_universe }
    if( thisgroup2 == "IDP") {
      reactLocal$N1 <- AppReactiveValue$idp_universe }
    if( thisgroup2 == "OOC") {
      reactLocal$N1 <- AppReactiveValue$ooc_universe }

    ## Calculate Sample size  #####
    reactLocal$samplesize  <- sampler::rsampcalc(N = reactLocal$N1,
                                      e = input$moe  ,
                                      ci = as.numeric(input$ci) * 100,
                                      p = input$p,
                                      over = input$over)
    #browser()
    set.seed(345)

    ## Dummy sample frame  #####
    reactLocal$dummysample <-  data.frame(
      employed = sample( c("Employed", "Unemployed"),
                         as.integer(reactLocal$samplesize),
                         replace = T,
                         prob=c( 0.35,0.65)),
      nationality = sample( c("Venezuela", "Haiti", "Cuba", "Other"),
                            as.integer(reactLocal$samplesize),
                            replace = T,
                            prob=c( 0.45,0.25,0.25,0.05)),
      gender = sample( c("Female", "Male"),
                       as.integer(reactLocal$samplesize),
                       replace = T,
                       prob=c( 0.45,0.55)),
      location = sample( c("Province_1", "Province_2","Province_3","Province_4",
                           "Province_5", "Province_6", "Province_7","Province_8"),
                         as.integer(reactLocal$samplesize),
                         replace = T,
                         prob=c( 0.13,0.115,0.12,0.2,
                                 0.15,0.05,0.125,0.11)))|>
    dplyr::mutate( samplinguniverse =  reactLocal$N1 )

    #browser()
    ## Dummy sample object  #####
    reactLocal$dummysampleobj <-reactLocal$dummysample |>
      # simple random sample (SRS) #######
      srvyr::as_survey_design( fpc = samplinguniverse)  |>
      # Use `survey_prop` calculate the proportion in each group
      srvyr::group_by(employed) |>
      srvyr::summarise(pct = srvyr::survey_prop(na.rm = TRUE,
                                                level = as.numeric(input$ci),
                                                vartype = c("se", "ci", "var", "cv")))

    #browser()
    ## Plotting uncertainity   #####|>
    reactLocal$plotuncertain <- reactLocal$dummysampleobj |>
      ggplot(       aes(x = pct,
                        moe = pct_se,
                        y = employed)) +
      ungeviz::stat_confidence_density(fill = "lightblue",
                                       height = 0.8,
                                       # The confidence level used to calculate the moe statistic. This defaults to 0.95 (moe corresponds to 95% confidence interval).
                                       confidence = as.numeric(input$ci)  ) +
      geom_point(aes(x = pct), size = 2) +
      # geom_errorbarh(aes(xmin = pct_low,
      #                    xmax = pct_upp),
      #                height = 0.4) +

      geom_errorbarh(aes(xmin = pct - input$moe / 100,
                         xmax = pct + input$moe / 100),
                     height = 0.4, size = 2, color = "orange") +

      xlim(0, 1) +
      unhcrthemes::theme_unhcr(font_size = 14, rel_small = 6/9)+
      ggplot2::labs(title = paste0("Sample size of ",
                                      reactLocal$samplesize ,
                                      " covering a universe of ",
                                      prettyNum(  reactLocal$N1 ,big.mark=",", scientific=FALSE)),
                    subtitle = paste0(  "  Confidence level (blue): ",
                                      as.numeric(input$ci)*100 ,
                                      "% and margin of error (orange): ",
                                      input$moe , " %"),
                    x = "", y = "")
    ## et voila!!
    reactLocal$plotuncertain
  })



}

## copy to body.R
# mod_estimate_ui("estimate_ui_1")

## copy to sidebar.R
# shinydashboard::menuItem("displayName",tabName = "estimate",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_estimate_server, "estimate_ui_1", AppReactiveValue)

