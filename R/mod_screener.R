#' Module UI

#' @title mod_screener_ui and mod_screener_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @keywords internal

mod_screener_ui <- function(id, thisgroup) {
  ns <- NS(id)
  golem::activate_js()
  tabItem(tabName = "screener",
          fluidRow(
            column(
              width = 6,
              fluidRow(
                shinydashboard::box(
                  title = paste0("Initial Check for ", thisgroup,"- Tick to confirm if correct"),
                  #  status = "primary",
                  status = "info",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  #background = "light-blue",
                  width = 12,
                  fluidRow(
                    checkboxInput(
                      inputId = ns("availablereg"),
                      label = "I confirm that there is an up to date and complete registration list of the population group available.
          		    -Up to date- refers to a database which reflects the current population in the
          		  country. If the size or demographic composition of the population group has
          		  changed more than 10% since the last registration, it cannot be considered up to
          		  date.  -Complete- refers to a database which contains registration information for 80% or
          		  more of the total population group."),

                ## using div so that we can use invoke_js to display conditionally
                    div(
                      id = ns("target_lessthan5000"),
                      checkboxInput(
                        inputId = ns("lessthan5000"),
                        label = "The population group is smaller than 5,000 individuals,
              		  but the operations still want to use probabilistic sampling and confirm it as the has
              		  the resource, boh in terms of budget and time  to proceed with a
                    probabilistic approach for this population group")
                    ),
                ## Chec budget
                  div(
                    id = ns("target_budget"),
                    checkboxInput(
                      inputId = ns("budget"),
                      label = "There is specific budget and time available to conduct a
                  listing exercise IN ADDITION of actual survey data collection.
                  A household listing exercise can be used to construct a sampling frame but is time
        		consuming and expensive. A listing exercise requires either listing agents that count
        		every household within the specified boundary and create a list of all eligible
        		households or a system that calls automatically people (Random Digital Dial) and
        		gather basic information on them through Interactive Voice Response (IVR) or a Chatbot."
                    )
                  )
                  )
                ),
                shinydashboard::box(
                  title = "Population Accessibility",
                  #  status = "primary",
                  status = "info",
                  solidHeader = FALSE,
                  collapsible = FALSE,
                  #background = "light-blue",
                  width = 12,
                  fluidRow(
                    div(
                     id = ns("target_traceable"),
                     radioButtons(
                      inputId = ns("traceable"),
                      label = "Is the population group traceable?
        		  Traceable refers to population groups whose homes can be located or who can be
        		reached via telephone",
                      choices =  c(
                        "Unknown" = "unknown" ,
                        "Only Partially known (less than 80% of Population Group)" = "partknown"  ,
                        "Known (in camps or with regular registration verification)" = "known")
                    )
                  ),
                  div(
                    id = ns("target_spread"),
                    radioButtons(
                      inputId = ns("spread"),
                      label = "What is Geographic distribution (i.e. concentration) of that population group in the country?",
                      choices =  c(
                        "Scattered - all spread out within the country" = "scattered",

                        "Found only within a small area (for instance only a few
                        villages, cities, or camps easily accessible by
                        vehicle or on foot)" = "small",

                        "Concentrated within a bounded area(s) - live within
        		a geographic area with clearly identified boundaries, such as towns, villages,
        		neighbourhoods, settlements, camps or other geographic areas for which specific
        		boundaries can be clearly identified by a field team" = "concentrated",

                        "Both - some areas of known concentration together with
                        more scaterred situations" = "both"
                      )
                    )
                  ))
                ),
                shinydashboard::box(
                  title = "Disaggregation Requirement",
                  #  status = "primary",
                  status = "info",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  #background = "light-blue",
                  width = 12,
                  fluidRow(
                    div(
                     id = ns("target_strata"),
                     checkboxInput(
                      inputId = ns("strata"),
                      label = "The operation would like to benefit from statistically
                      reliable disaggregation
                  and is considering sample Stratification (aka setting up strata).
        		  Strata are distinct subgroups of the target population. Each population group
        		can  be further split into different strata. For example, RAS and IDPs may be
        		divided  into two distinct subgroups, or stratum: 1. RAS living in camps;
        		2. RAS living  outside of camps, or 1. Rural IDPs; 2. Urban IDPs,"
                    )
                  )
                  ## If - display what Stratification could make sense based on available disaggregation within ASR
                  ## Location - Nationality - Gender - location type)
                 )
                ),
                shinydashboard::box(
                    title = "Follow up questions in the absence of sampling frame...",
                    #  status = "primary",
                    status = "info",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    #background = "light-blue",
                    width = 12,
                    fluidRow(
                      div(
                        id = ns("target_gather"),
                        checkboxInput(
                          inputId = ns("gather"),
                          label = "The population tend to gather at a certain location on a specific day/time?
      		  The population may habitually gather for certain ceremonies, festivities, or events
      that are well known. In case these are known in advance and all population
      members have the possibility to be present at the location, these days and times
      can be opportunities to survey the population and. For example, the population
      might gather for a humanitarian distribution or a registration exercise."  )
                      ),
                      div(
                        id = ns("target_network"),
                        checkboxInput(
                          inputId = ns("network"),
                          label = "The population a well-connected community, where people tend to know each others")
                      )
                      ,
                      div(
                        id = ns("target_expert"),
                        checkboxInput(
                          inputId = ns("expert"),
                          label = "There is adequate time and expertise to carry out a
      		  Respondent Driven Sampling (RDS) approach, which entails conducting a
      formative survey to identify seeds and prepare RDS coupons?"
                        )
                      )
                    )
                  )
               )
              ),

              column(
                width = 6,
                shinydashboard::box(
                  title = paste0("Recommended Sampling Approach for ", thisgroup),
                  #  status = "primary",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  #background = "light-blue",
                  width = 12,
                  tags$head(
                    tags$style(
                      HTML(
                        "#shiny-text-output  {
                          color:white;
                          font-size:12px;
                          font-style:italic;
                          overflow-y:scroll;
                          max-height: 700px;
                          background: \"#0072BC\";
                          }"
                      )
                    )
                  ),
                  #div(style = "color:white; font-size:12px; font-style:italic; overflow-y:scroll; max-height: 700px; background: \"#0072BC\";",
                      fluidRow(verbatimTextOutput(
                        outputId = ns("sampling"),
                        placeholder = TRUE
                        )
                   # )
                    )
                ))
            )
  )

}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @keywords internal

mod_screener_server <- function(input, output, session, AppReactiveValue, thisgroup) {
  ns <- session$ns
  # Sampling approach description ####
  nonprob <-
"Non-probabilistic methods

Those methods do not use random selection to choose survey participants and therefore suffer
from sampling biases.
Indicator estimates derived from such methods will NOT be statistically representative
of the concerned population and should be seen as a last resort to generate indicator data.

These may include conducting surveys using

  (a) Convenience sampling requires little or no planning and involves selecting respondents
  who are readily available. An example of convenience sampling would include interviewing
  households who are present in the street or a public building.

  (b) Quota sampling, as the name suggests, involves determining a certain number of
  respondents from each study group (i.e., population groups such as refugees and asylum
  seekers, IDPs) and conveniently interviewing households from each group.

  (c) Snowball sampling involves selecting respondents randomly or purposefully in the
  beginning and asking for referrals"


  srs <-
"Simple Random Sampling (SRS) without stratification.

SRS gives each population group an equal chance of being selected for the sample."

  ppswalk <-
"Multiple-Stage Cluster Sampling
Probability Proportion to Size (PPS) from random walk

If the population is concentrated within clusters and clusters tend to be found in the same
region or geographic area.

After selecting clusters based on their population size, enumerators survey every nth
household until the required number of households is selected.

For example, enumerators may visit every third household within the cluster."

  ppsframe <-
"Multiple-Stage Cluster Sampling
with Probability Proportion to Size (PPS) from sample frame.

After selecting clusters based on their population size, Use the sample frame created from
the listing exercise to randomly select households."

  srsstrata <-
"Simple Random Sampling (SRS) within Strata.

SRS gives each group within a stratum an equal chance of being selected for the sample."


  acs <-
"Adaptive Cluster Sampling

Thismethod first divides the population into a grid of plots and randomly selects a few
initial plots.

The selected plots are reviewed to determine whether the population is present or not –
a condition required to select the plot.

If there are a minimum number of households of the population group present in the plot,
the research team also reviews any neighbouring plots to ensure the minimum number of
households of the population group are present.

Each plot that has the minimum number of households of the population group is selected
until there are no additional neighbouring plots with such households present.

Clusters of plots are then generated where the survey will take place. This method may
be most useful when the population is very concentrated in certain plots, but these plots
are widely dispersed."

  lts <-
"Time Location Sampling.

Identify times and locations where the population is likely to gather and select the times
and locations where the greatest number will be present.

Conduct interviews on location."

  rds <-
"Respondent Driven Sampling (RDS).

Identify initial ‘seed’ population who are well connected in their community and can refer
additional households of the same population group to the survey.

Respondents will continue to recruit additional households until the desired sample size is
reached.

Additional data protection considerations need to be taken when adopting RDS as seeds will
be sharing personal data of other potential survey respondents.

Carrying out a Respondent Driven Sampling (RDS) approach, entails conducting a formative
survey to identify seeds and prepare RDS coupons"


  reactLocal <- reactiveValues(
    availablereg = FALSE  ,
    budget = FALSE  ,
    lessthan5000 = FALSE  ,
    traceable = NULL  ,
    spread = NULL  ,
    strata = NULL  ,
    gather = NULL  ,
    network = NULL  ,
    expert = NULL  ,
    sampling = "We are not yet able to define the adequate sampling approach"  ,
    ## conditional display
    show_lessthan5000 = FALSE  ,
    show_budget = TRUE  ,  ## always on
    show_traceable= TRUE  , ## always on
    show_spread = TRUE  , ## always on
    show_strata = FALSE  , # Dilay only if Reg or Budget is TRUE
    show_gather = FALSE  ,
    show_network = FALSE  ,
    show_expert = FALSE
  )

  # Skip logic ####

  ### availablereg ####
  observeEvent(eventExpr = input$availablereg, {
    reactLocal$availablereg <- input$availablereg
    if( reactLocal$availablereg == TRUE) {
       reactLocal$show_strata <- TRUE
    } else if( reactLocal$availablereg == FALSE) {
      reactLocal$show_strata <- FALSE    }

  })
  ### budget ####
  observeEvent(eventExpr = input$budget, {
    reactLocal$budget <- input$budget
    if( reactLocal$budget == TRUE) {
      reactLocal$show_strata <- TRUE
    } else if( reactLocal$availablereg == FALSE) {
      reactLocal$show_strata <- FALSE    }
  })

  ### lessthan5000 ####
  ## This tobe displayed only if the population is actually less
  ## Reinject int the app reactive value
  observeEvent(reactLocal$show_lessthan5000, {
  if( all(thisgroup == "RAS" & isTRUE(AppReactiveValue$ras_islessthan5000)))  {
    reactLocal$show_lessthan5000 <- TRUE  }
    if( all( thisgroup == "STA" & isTRUE(AppReactiveValue$sta_islessthan5000)))  {
    reactLocal$show_lessthan5000 <- TRUE  }
    if( all( thisgroup == "RET" & isTRUE(AppReactiveValue$ret_islessthan5000 )))  {
    reactLocal$show_lessthan5000 <- TRUE  }
    if( all( thisgroup == "IDP"& isTRUE(AppReactiveValue$idp_islessthan5000 )))  {
    reactLocal$show_lessthan5000 <- TRUE  }
    if( all(thisgroup == "OOC"& isTRUE(AppReactiveValue$ooc_islessthan5000 )))  {
    reactLocal$show_lessthan5000 <- TRUE  }
    #browser()
    if(isTRUE(reactLocal$show_lessthan5000)) {
      golem::invoke_js("show", paste0("#", ns("target_lessthan5000")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("target_lessthan5000")))
    }
  })
  observeEvent(eventExpr = input$lessthan5000, {
    reactLocal$lessthan5000 <- input$lessthan5000
  })


  ### traceable ####
  observeEvent(eventExpr = input$traceable, {
    reactLocal$traceable <- input$traceable

    ## define path for quasi-probablity sampling -
    if( all( reactLocal$availablereg == FALSE &
             !(reactLocal$spread =="small") &
             input$traceable %in% c("unknown","partknown" )) )  {
      reactLocal$show_gather <- TRUE
    } else {  reactLocal$show_gather <- FALSE}

  })


  ### spread ####
  observeEvent(eventExpr = input$spread, {
    reactLocal$spread <- input$spread
    ## define path for quasi-probablity sampling -
    if( all( reactLocal$availablereg == FALSE &
        !(reactLocal$spread =="small") &
        input$traceable %in% c("unknown","partknown" )) )  {
      reactLocal$show_gather <- TRUE
    } else {  reactLocal$show_gather <- FALSE}

  })

  ### strata ####
  ## Displayed only an initial Reg is available...
  observeEvent(reactLocal$show_strata, {
    if(isTRUE(reactLocal$show_strata)) {
      golem::invoke_js("show", paste0("#", ns("target_strata")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("target_strata")))
    }
  })
  observeEvent(eventExpr = input$strata, {
    reactLocal$strata <- input$strata
  })

  ### gather ####
  ## Beginning of quasi probability...
  observeEvent(eventExpr = input$gather, {
    reactLocal$gather <- input$gather
    if( reactLocal$gather == FALSE) {
      reactLocal$show_network <- FALSE
    } else if ( reactLocal$gather == TRUE) {
      reactLocal$show_network <- TRUE
    }
  })
  observeEvent(reactLocal$show_gather, {
    if(isTRUE(reactLocal$show_gather)) {
      golem::invoke_js("show", paste0("#", ns("target_gather")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("target_gather")))
    }
  })

  ### network ####
  observeEvent(reactLocal$show_network, {
    if(isTRUE(reactLocal$show_network)) {
      golem::invoke_js("show", paste0("#", ns("target_network")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("target_network")))
    }
  })
  observeEvent(eventExpr = input$network, {
    reactLocal$network <- input$network

    if( reactLocal$network == TRUE) {
      reactLocal$show_expert <- TRUE
    } else if ( reactLocal$network == FALSE) {
      reactLocal$show_expert <- FALSE
    }

  })

  ### expert ####
  observeEvent(reactLocal$show_expert, {
    if(isTRUE(reactLocal$show_expert)) {
      golem::invoke_js("show", paste0("#", ns("target_expert")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("target_expert")))
    }
  })
  observeEvent(eventExpr = input$expert, {
    reactLocal$expert <- input$expert
  })

  # Sampling Tree ##############
  output$sampling <- renderText({

    validate(
      need(isTruthy( reactLocal$sampling),
           message = "We are not yet able to define the adequate sampling approach")
    )

    # based on decision tree
      #  reactLocal$lessthan5000 == TRUE  &
      #   reactLocal$spread  %in% c("unknown" ) *
      #   reactLocal$strata == TRUE  &
      #   reactLocal$show_budget == TRUE  &
      #   reactLocal$show_gather == TRUE  &
      #   reactLocal$show_expert  == TRUE


    reactLocal$sampling <- dplyr::case_when(

      ##  1 - nonprob  ########
     (
       reactLocal$availablereg == FALSE &
        reactLocal$traceable %in% c("unknown","partknown" )  #  "unknown" , "partknown"  ,  "known"
       # reactLocal$spread   #  "scattered",  "small",  "concentrated",  "both"
       # reactLocal$strata
       # reactLocal$budget
       # reactLocal$gather
       # reactLocal$expert
                                             )  ~ nonprob,
     ## 2 - Quasiprob  ########
     ### lts  ########
     reactLocal$availablereg == FALSE &
       !(reactLocal$spread == "small") &
       reactLocal$traceable %in% c("unknown","partknown" ) &
       reactLocal$gather == TRUE ~ lts,

     ### rds  ########
     reactLocal$availablereg == FALSE &
       !(reactLocal$spread == "small") &
       reactLocal$traceable %in% c("unknown","partknown" ) &
       reactLocal$gather == FALSE &
       reactLocal$expert == TRUE ~ rds,

     ## 3- Prob  ########

      ### srs ########
      (reactLocal$availablereg == TRUE &
         reactLocal$traceable %in% c("known" ) &
         reactLocal$spread %in% c("concentrated")   ) ~ srs,

      ### ppswalk ########
      (reactLocal$budget == TRUE &
         reactLocal$spread %in% c("concentrated")) == TRUE ~ ppswalk,
      ### ppsframe ########
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,

      ### srsstrata ########
      reactLocal$strata == TRUE ~ srsstrata,

      ### acs ########
     ( reactLocal$budget == TRUE &
         reactLocal$spread %in% c("scattered")) ~ acs,


     ## caramba...
      TRUE ~ "Ay caramba!... We are not yet able to define the adequate sampling approach")
   # browser()


    ## Reinject int the app reactive value
    if( thisgroup == "RAS") {
      AppReactiveValue$ras_sampling <- reactLocal$sampling
      AppReactiveValue$ras_availablereg <- reactLocal$availablereg
      AppReactiveValue$ras_lessthan5000  <- reactLocal$lessthan5000
      AppReactiveValue$ras_spread  <- reactLocal$spread
      AppReactiveValue$ras_traceable  <- reactLocal$traceable
      AppReactiveValue$ras_strata  <- reactLocal$strata
      AppReactiveValue$ras_budget  <- reactLocal$budget
      AppReactiveValue$ras_gather  <- reactLocal$gather
      AppReactiveValue$ras_expert   <- reactLocal$expert
    }
    if( thisgroup == "STA") {
      AppReactiveValue$sta_sampling <- reactLocal$sampling
      AppReactiveValue$sta_availablereg <- reactLocal$availablereg
      AppReactiveValue$sta_lessthan5000  <- reactLocal$lessthan5000
      AppReactiveValue$sta_spread  <- reactLocal$spread
      AppReactiveValue$sta_traceable  <- reactLocal$traceable
      AppReactiveValue$sta_strata  <- reactLocal$strata
      AppReactiveValue$sta_budget  <- reactLocal$budget
      AppReactiveValue$sta_gather  <- reactLocal$gather
      AppReactiveValue$sta_expert   <- reactLocal$expert
    }
    if( thisgroup == "RET") {
      AppReactiveValue$ret_sampling <- reactLocal$sampling
      AppReactiveValue$ret_availablereg <- reactLocal$availablereg
      AppReactiveValue$ret_lessthan5000  <- reactLocal$lessthan5000
      AppReactiveValue$ret_spread  <- reactLocal$spread
      AppReactiveValue$ret_traceable  <- reactLocal$traceable
      AppReactiveValue$ret_strata  <- reactLocal$strata
      AppReactiveValue$ret_budget  <- reactLocal$budget
      AppReactiveValue$ret_gather  <- reactLocal$gather
      AppReactiveValue$ret_expert   <- reactLocal$expert
    }
    if( thisgroup == "IDP") {
      AppReactiveValue$idp_sampling <- reactLocal$sampling
      AppReactiveValue$idp_availablereg <- reactLocal$availablereg
      AppReactiveValue$idp_lessthan5000  <- reactLocal$lessthan5000
      AppReactiveValue$idp_spread  <- reactLocal$spread
      AppReactiveValue$idp_traceable  <- reactLocal$traceable
      AppReactiveValue$idp_strata  <- reactLocal$strata
      AppReactiveValue$idp_budget  <- reactLocal$budget
      AppReactiveValue$idp_gather  <- reactLocal$gather
      AppReactiveValue$idp_expert   <- reactLocal$expert
    }
    if( thisgroup == "OOC") {
      AppReactiveValue$ooc_sampling <- reactLocal$sampling
      AppReactiveValue$ooc_availablereg <- reactLocal$availablereg
      AppReactiveValue$ooc_lessthan5000  <- reactLocal$lessthan5000
      AppReactiveValue$ooc_spread  <- reactLocal$spread
      AppReactiveValue$ooc_traceable  <- reactLocal$traceable
      AppReactiveValue$ooc_strata  <- reactLocal$strata
      AppReactiveValue$ooc_budget  <- reactLocal$budget
      AppReactiveValue$ooc_gather  <- reactLocal$gather
      AppReactiveValue$ooc_expert   <- reactLocal$expert
    }

    ## Display it on the GUI
    reactLocal$sampling

    })
}

## copy to body.R
# mod_screener_ui("screener_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "screener",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_screener_server, "screener_ui_1", AppReactiveValue)

