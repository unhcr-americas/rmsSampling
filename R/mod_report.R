#' Module UI

#' @title mod_report_ui and mod_report_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @importFrom DiagrammeR grVizOutput
#' @keywords internal

mod_report_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "report",
		fluidRow(
		  fluidRow(
		    column(
		      width = 12,
		      h2('Sampling Design Documentation'),
		      p("As you have finalised all the steps before, you can now download the
		        documentation to include within your survey scoping note."),
		      p("
		        You shall also upload this document as a ressource attachment witin the ",
		            tags$a(href="https://ridl.unhcr.org/",
		                 "RIDL dataset"),
		            "space created for your RMS (check here for ",
		        tags$a(href="https://im.unhcr.org/ridl",
		               "RIDL Documentation and Tutorial"),
		        ")."),
		      p("In case, you have an enumeration list, do not hesitate to reach out
		        to your regional DIMA to obtain support for the sample drawing script (see ",
		        tags$a(href="https://unhcr365-my.sharepoint.com/:w:/r/personal/legoupil_unhcr_org/Documents/Sample_Drawing-RMS-ProGres_wordv1.docx?d=wa636c64ac0b14e798cb032efbb7ebf8f&csf=1&web=1&e=iznEeB",
		               "here an example"),
		        " for Progres sample drawing).")
		    )
		  ),

		  fluidRow(
		    shinydashboard::box(
		      title = "Report",
		      #  status = "primary",
		      status = "info",
		      solidHeader = FALSE,
		      collapsible = TRUE,
		      #background = "light-blue",
		      width = 12,
		      downloadButton(outputId = ns("showreport"),
		                     label = "Download your RMS Methodological Approach Documentation",
		                     width =  "400px")  ,
		      hr(),

		      DiagrammeR::grVizOutput( outputId = ns("step_by_step"),
		                               width = "100%",
		                               height = "800px")
		    )
		  )

		)
	)
}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @importFrom DiagrammeR renderGrViz grViz
#' @keywords internal

mod_report_server <- function(input, output, session, AppReactiveValue) {
	ns <- session$ns

	output$showreport <- downloadHandler(
	  # For PDF output, change this to "report.pdf"
	 # filename = "sample_design_report.docx",
	  filename =  function() {paste0("RMS_Methodological_Approach_",AppReactiveValue$country,"_",
	                                 format(Sys.Date(), '%Y'),".docx")},
	  content = function(file) {
	    # Copy the report file to a temporary directory before processing it, in
	    # case we don't have write permissions to the current working dir (which
	    # can happen when deployed).
	    tempReport <- file.path(tempdir(), "report.Rmd")
	    file.copy(system.file("rmarkdown/templates/sample_design_report/skeleton/skeleton.Rmd",
	                          package = "rmsSampling"),
	              tempReport, overwrite = TRUE)

	    #browser()
	    # Set up parameters to pass to Rmd document
	    params = list(
	      country = AppReactiveValue$country,
	     # poptype = AppReactiveValue$country,
	      ## Sample info for ras
	      ras_mode = AppReactiveValue$ras_mode,
	      ras_universe = AppReactiveValue$ras_universe,
	      ras_sampling = AppReactiveValue$ras_sampling,
	      ras_availablereg  = AppReactiveValue$ras_availablereg,
	      ras_lessthan5000  = AppReactiveValue$ras_lessthan5000,
	      ras_spread  = AppReactiveValue$ras_spread,
	      ras_traceable  = AppReactiveValue$ras_traceable,
	      ras_strata  = AppReactiveValue$ras_strata,
	      ras_budget  = AppReactiveValue$ras_budget,
	      ras_gather  = AppReactiveValue$ras_gather,
	      ras_expert   = AppReactiveValue$ras_expert,
	      ras_samplesize   = AppReactiveValue$ras_samplesize,
	      ras_ci   = AppReactiveValue$ras_ci,
	      ras_moe   = AppReactiveValue$ras_moe,
	      ras_p   = AppReactiveValue$ras_p,
	      ras_power   = AppReactiveValue$ras_power,
	      ras_over   = AppReactiveValue$ras_over,

	      ## Sample info for sta
	      sta_mode = AppReactiveValue$sta_mode,
	      sta_universe = AppReactiveValue$sta_universe,
	      sta_sampling = AppReactiveValue$sta_sampling,
	      sta_availablereg  = AppReactiveValue$sta_availablereg,
	      sta_lessthan5000  = AppReactiveValue$sta_lessthan5000,
	      sta_spread  = AppReactiveValue$sta_spread,
	      sta_traceable  = AppReactiveValue$sta_traceable,
	      sta_strata  = AppReactiveValue$sta_strata,
	      sta_budget  = AppReactiveValue$sta_budget,
	      sta_gather  = AppReactiveValue$sta_gather,
	      sta_expert   = AppReactiveValue$sta_expert,
	      sta_samplesize   = AppReactiveValue$sta_samplesize,
	      sta_ci   = AppReactiveValue$sta_ci,
	      sta_moe   = AppReactiveValue$sta_moe,
	      sta_p   = AppReactiveValue$sta_p,
	      sta_power   = AppReactiveValue$sta_power,
	      sta_over   = AppReactiveValue$sta_over,

	      ## Sample info for ret
	      ret_mode = AppReactiveValue$ret_mode,
	      ret_universe = AppReactiveValue$ret_universe,
	      ret_sampling = AppReactiveValue$ret_sampling,
	      ret_availablereg  = AppReactiveValue$ret_availablereg,
	      ret_lessthan5000  = AppReactiveValue$ret_lessthan5000,
	      ret_spread  = AppReactiveValue$ret_spread,
	      ret_traceable  = AppReactiveValue$ret_traceable,
	      ret_strata  = AppReactiveValue$ret_strata,
	      ret_budget  = AppReactiveValue$ret_budget,
	      ret_gather  = AppReactiveValue$ret_gather,
	      ret_expert   = AppReactiveValue$ret_expert,
	      ret_samplesize   = AppReactiveValue$ret_samplesize,
	      ret_ci   = AppReactiveValue$ret_ci,
	      ret_moe   = AppReactiveValue$ret_moe,
	      ret_p   = AppReactiveValue$ret_p,
	      ret_power   = AppReactiveValue$ret_power,
	      ret_over   = AppReactiveValue$ret_over,

	      ## Sample info for idp
	      idp_mode = AppReactiveValue$idp_mode,
	      idp_universe = AppReactiveValue$idp_universe,
	      idp_sampling = AppReactiveValue$idp_sampling,
	      idp_availablereg  = AppReactiveValue$idp_availablereg,
	      idp_lessthan5000  = AppReactiveValue$idp_lessthan5000,
	      idp_spread  = AppReactiveValue$idp_spread,
	      idp_traceable  = AppReactiveValue$idp_traceable,
	      idp_strata  = AppReactiveValue$idp_strata,
	      idp_budget  = AppReactiveValue$idp_budget,
	      idp_gather  = AppReactiveValue$idp_gather,
	      idp_expert   = AppReactiveValue$idp_expert,
	      idp_samplesize   = AppReactiveValue$idp_samplesize,
	      idp_ci   = AppReactiveValue$idp_ci,
	      idp_moe   = AppReactiveValue$idp_moe,
	      idp_p   = AppReactiveValue$idp_p,
	      idp_power   = AppReactiveValue$idp_power,
	      idp_over   = AppReactiveValue$idp_over,

	      ## Sample info for ooc
	      ooc_mode = AppReactiveValue$ooc_mode,
	      ooc_universe = AppReactiveValue$ooc_universe,
	      ooc_sampling = AppReactiveValue$ooc_sampling,
	      ooc_availablereg  = AppReactiveValue$ooc_availablereg,
	      ooc_lessthan5000  = AppReactiveValue$ooc_lessthan5000,
	      ooc_spread  = AppReactiveValue$ooc_spread,
	      ooc_traceable  = AppReactiveValue$ooc_traceable,
	      ooc_strata  = AppReactiveValue$ooc_strata,
	      ooc_budget  = AppReactiveValue$ooc_budget,
	      ooc_gather  = AppReactiveValue$ooc_gather,
	      ooc_expert   = AppReactiveValue$ooc_expert,
	      ooc_samplesize   = AppReactiveValue$ooc_samplesize,
	      ooc_ci   = AppReactiveValue$ooc_ci,
	      ooc_moe   = AppReactiveValue$ooc_moe,
	      ooc_p   = AppReactiveValue$ooc_p,
	      ooc_power   = AppReactiveValue$ooc_power,
	      ooc_over   = AppReactiveValue$ooc_over
	       )
	    id <- showNotification(
	      "Rendering report...",
	      duration = NULL,
	      closeButton = FALSE
	    )
	    on.exit(removeNotification(id), add = TRUE)
	    # Knit the document, passing in the `params` list, and eval it in a
	    # child of the global environment (this isolates the code in the document
	    # from the code in this app).
	    rmarkdown::render(tempReport,
	                      output_file = file,
	                      params = params,
	                      envir = new.env(parent = globalenv())
	    )
	  }
	)


	output$step_by_step <- DiagrammeR::renderGrViz({

	  ## Nice tutorial here - https://sketchviz.com/flowcharts-in-graphviz
	  DiagrammeR::grViz(
	    "digraph flowchart {

  /* graphviz to charts is what latex is to documents,
 * sometimes you'll have to fight it.
 * This is typically done by defining levels and connection points that
 * don't really have anything to do with your graph, but are used to
 * force the graph to appear in a certain way.
 * See also - https://rich-iannone.github.io/DiagrammeR/articles/graphviz-mermaid.html
 * https://stackoverflow.com/questions/7115870/creating-straight-edges-in-graphviz/12869546
 */

  # node attributes
  node [fontname = Lato];
  edge [fontname = Lato];


 # Decision Points  #########
 facecondition [
    label = 'Q: There is possibility to do \n face to face interview';
    shape = diamond;
    color = '#FAEB00';
    style = rounded;
  ];

 lessthan5000 [
    label = 'Q: The population group is \n smaller than 5,000 individuals';
    shape = diamond;
    color = '#FAEB00';
  ];

  availablereg [
    label = 'Q: Up to date and \n complete registration list';
	  shape = diamond;
    color = '#FAEB00';
	    ];

  budget [
    label = 'Q: Specific budget and time available \nto conduct a listing exercise ';
    shape = diamond;
    color = '#FAEB00';
   ];

	traceable [
    label = 'Q: Population group traceable';
	  shape = diamond;
    color = '#FAEB00';
	  ];

  spread [
    label = 'Q: Geographic distribution \n of that population group in the country ';
    shape = diamond;
    color = '#FAEB00';
   ];

  cluster [
    label = 'Q: Do we have already  \n population size breakdown by area within the country? ';
    shape = diamond;
    color = '#FAEB00';
   ];

   strata [
    label = 'Q: Operation need \nfrom statistically reliable disaggregation ';
	  shape = diamond;
    color = '#FAEB00';
	 ];

	gather [
    label = 'Q: Population tend to gather at \n a certain location on a specific day/time ';
    shape = diamond;
    color = '#FAEB00';
   ];

  network [
    label = 'Q: Population is a well-connected community,\n where people tend to know each others ';
    shape = diamond;
    color = '#FAEB00';
   ];

   expert [
    label = 'Q: Adequate time and expertise to \ncarry out Respondent Driven Sampling';
    shape = diamond;
    color = '#FAEB00';
  ];


  # Method Statement #########
  nonprob [
    label = 'Non-probabilistic methods';
    shape = rect;
    color = '#99c7e4';
  ];

  srs [
    label = 'Simple Random Sampling \n(SRS) without stratification';
    shape = rect;
    color = '#338ec9';
  ];



  ppsframe  [
    label = 'Multiple-Stage \nCluster Sampling\n with Frame';
    shape = rect;
    color = '#338ec9';
  ];

  srsstrata  [
    label = 'Simple Random Sampling\n (SRS) within Strata';
    shape = rect;
    color = '#338ec9';
  ];

  acs [
    label = 'Adaptive Cluster Sampling';
    shape = rect;
    color = '#338ec9';
  ];

  lts [
    label = 'Time Location Sampling';
    shape = rect;
    color = '#66aad7';
  ];

  rds [
    label = 'Respondent Driven Sampling\n (RDS)';
    shape = rect;
    color = '#66aad7';
  ];


/*
 * tricky part:
 * since nodes in a digrap go either from top to bottom or left to right, we
 * can usually not connect (->) two nodes and have them appear on the same
 * level unless the connection is specified within a block that has the
 * parameter `rank' set to `same'
 * See also - https://sketchviz.com/flowcharts-in-graphviz
 */


  # edge statements  #########
  lessthan5000 -> facecondition;
  lessthan5000 -> nonprob[ label = 'Yes' ];
  facecondition ->  availablereg;

  availablereg -> traceable[ label = 'Yes' ];



  traceable -> budget[ label = 'No' ];
  traceable -> spread[ label = 'Yes' ];


  budget -> nonprob[ label = 'No' ];
  budget -> strata[ label = 'Yes' ];

  spread -> srs[ label = 'Concentrated' ];
  spread -> budget[ label = 'Scattered' ];

  strata ->  srsstrata[ label = 'Yes' ];

  strata ->  cluster[ label = 'No' ];
  cluster ->  acs[ label = 'No' ];
  cluster ->  ppsframe[ label = 'Yes' ];



  # edge statements -- Pseudo Probabilistic  #########
	availablereg ->  gather[ label = 'No' ];
	gather -> lts[ label = 'Yes' ];

  gather -> network[ label = 'No' ];

  network -> expert[ label = 'Yes' ];
  network -> nonprob[ label = 'No' ];
  expert -> rds[ label = 'Yes' ];
  expert -> nonprob[ label = 'No' ];

} ")
	})


}



# # node type 1: starting and ending node
# node [fontname = Lato, shape = oval]
# start [label = '@@1']
# node [fontname = Lato, shape = oval]
# pro2 [label = '@@6']
#
# # node type 2: decision
# node [fontname = Lato, shape = diamond]
# dec1 [label = '@@2']
# dec2 [label = '@@3']
# node [fontname = Lato, shape = diamond]
# dec3 [label = '@@4']
#
# # node type 3: process
# node [fontname = Lato, shape = rectangle]
# pro1 [label = '@@5']
# node [fontname = Lato, shape = rectangle]
# pro3 [label = '@@7']
#
# # specify which nodes are of the same 'rank' so that they'll be drawn at the same level
# {rank = same; dec2 pro3}
# {rank = same; pro1 dec3}
#
# # edge definitions with the node IDs
# edge[tailclip = true, headclip = true];
# start -> dec1
# dec1 -> dec2 [fontname = Lato, label = '', headport = 'n', tailport = 's']
# dec1 -> pro3 [fontname = Lato, label = '', headport = 'n', tailport = 'e']
# dec2 -> pro1 [fontname = Lato, label = '', tailport = 's']
# dec2 -> pro3 [fontname = Lato, label = '']
# pro3 -> dec3 [fontname = Lato, label = '', headport = 'n']
# dec3 -> pro2 [fontname = Lato, label = '', headport = 'n', tailport = 's']
# dec3 -> pro3 [fontname = Lato, label = '', headport = 'e', tailport = 'e']
# pro1 -> dec1 [fontname = Lato, label = '', headport = 'w']
# }
#
# [1]: 'START'
# [2]: 'Data Collection Mode'
# [3]: 'Assessment of Sampling Frame'
# [4]: 'Assessment of Population Spatial Dispersion'
# [5]: 'process 1'
# [6]: 'END'
# [7]: 'process 3'

## copy to body.R
# mod_report_ui("report_ui_1")

## copy to sidebar.R
# shinydashboard::menuItem("displayName",tabName = "report",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_report_server, "report_ui_1", AppReactiveValue)

