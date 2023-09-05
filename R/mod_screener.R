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

mod_screener_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "screener",
    fluidRow(
      shinydashboard::box(
        title = "Screening Questions",
        #  status = "primary",
        status = "info",
        solidHeader = FALSE,
        collapsible = TRUE,
        #background = "light-blue",
        width = 12,
        fluidRow(
          shinydashboard::box(
            title = "Initial Check - thick to confirm if correct",
            #  status = "primary",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            #background = "light-blue",
            width = 6,
            fluidRow(
              checkboxInput(
                inputId = ns("availablereg"),
                label = "I confirm that there is an up to date and complete registration list of the population group available.
          		    -Up to date- refers to a database which reflects the current population in the
          		  country. If the size or demographic composition of the population group has
          		  changed more than 10% since the last registration, it cannot be considered up to
          		  date.  -Complete- refers to a database which contains registration information for 80% or
          		  more of the total population group."
              ),
              checkboxInput(
                inputId = ns("lessthan5000"),
                label = "The population group is smaller than 5,000 individuals,
          		  but the operations still want to use probabilistic sampling and confirm it as the has
          		  the resource, boh in terms of budget and time  to proceed with a
                probabilistic approach for this population group"
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
            width = 6,
            fluidRow(
              radioButtons(
                inputId = ns("traceable"),
                label = "Is the population group traceable?
      		  Traceable refers to population groups whose homes can be located or who can be
      		reached via telephone",
                choices =  c("Known (in camps)" = "known",
                             "Partially known" = "partknown",
                             "Unknown" = "unknown")   ),
              radioButtons(
                inputId = ns("spread"),
                label = "What is Geographic distribution (i.e. concentration) of that population group in the country?",
                choices =  c(
                  "Found only within a small area (for instance only a few villages, cities, or camps easily accessible by vehicle or on foot)" = "small",
                  "Scattered - spread out within the country" = "scattered",
                  "Concentrated within a bounded area(s) - live within
      		a geographic area with clearly identified boundaries, such as towns, villages,
      		neighbourhoods, settlements, camps or other geographic areas for which specific
      		boundaries can be clearly identified by a field team" = "concentrated",
                             "Both - some area of known concentration together with more scaterred situation" = "both") )

            )
          ),
          shinydashboard::box(
            title = "Disaggregaion Requirement",
            #  status = "primary",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            #background = "light-blue",
            width = 12,
            fluidRow(

              checkboxInput(
                inputId = ns("strata"),
                label = "The operation would like to benefit from statistically reliable disaggregation
                and is considering sample Stratification (aka setting up strata).
      		  Strata are distinct subgroups of the target population. Each population group
      		can  be further split into different strata. For example, RAS and IDPs may be
      		divided  into two distinct subgroups, or stratum: 1. RAS living in camps;
      		2. RAS living  outside of camps, or 1. Rural IDPs; 2. Urban IDPs,"
              ),
              ## If - display what Stratification could make sense based on available disaggregation within ASR
              ## Location - Nationality - Gender - location type
            )
          ),
          shinydashboard::box(
            title = "Additional details",
            #  status = "primary",
            status = "info",
            solidHeader = FALSE,
            collapsible = TRUE,
            #background = "light-blue",
            width = 12,
            fluidRow(
              checkboxInput(
                inputId = ns("budget"),
                label = "There is specific budget and time available to conduct a
                listing exercise in addition of survey data collection.
                A household listing exercise can be used to construct a sampling frame but is time
      		consuming and expensive. A listing exercise requires either listing agents that count
      		every household within the specified boundary and create a list of all eligible
      		households or a system that calls automatically people (Random Digital Dial) and
      		gather basic information on them through Interactive Voice Response (IVR) or a Chatbot."
              ),
         checkboxInput(
                inputId = ns("gather"),
                label = "The population tend to gather at a certain location on a specific day/time?
      		  The population may habitually gather for certain ceremonies, festivities, or events
      that are well known. In case these are known in advance and all population
      members have the possibility to be present at the location, these days and times
      can be opportunities to survey the population and. For example, the population
      might gather for a humanitarian distribution or a registration exercise."
              ),
              checkboxInput(
                inputId = ns("network"),
                label = "The population a well-connected community, where people tend to know each others"
              ),

              checkboxInput(
                inputId = ns("expert"),
                label = "There is adequate time and expertise to carry out a
      		  Respondent Driven Sampling (RDS) approach, which entails conducting a
      formative survey to identify seeds and prepare RDS coupons?"	)


            )
          ),
          shinydashboard::box(
            title = "RecommendedSampling Approach",
            #  status = "primary",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            #background = "light-blue",
            width = 12,
            fluidRow(
                verbatimTextOutput( outputId = ns("sampling"),
                                    placeholder = TRUE)
            )
          )




            )
          )
        ) ##End fluid row
      )
}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @keywords internal

mod_screener_server <- function(input, output, session, AppReactiveValue, thisgroup) {
  ns <- session$ns
  # Potential sampling approach with description as per the doc ####
  nonprob <- "Non-probabilistic methods do not use random selection to choose survey participants and therefore suffer from sampling biases.
	 Indicator estimates derived from such methods will not be statistically representative of the concerned population and should be seen as a last resort to generate indicator data.

	 These may include conducting surveys using convenience sampling, quota sampling, or snowball sampling.

     (a) Convenience sampling requires little or no planning and involves selecting respondents who are readily available. An example of convenience sampling would include interviewing households who are present in the
street or a public building.

     (b) Quota sampling, as the name suggests, involves determining a certain number of respondents from each study group (i.e., population groups such as refugees and asylum seekers, IDPs) and conveniently interviewing households from each group.

     (c) Snowball sampling involves selecting respondents randomly or purposefully in the beginning and asking for referrals"


  srs <- "Simple Random Sampling (SRS) without stratification.
  SRS gives each population group an equal chance of being selected for the sample."

  ppswalk <- "Multiple-Stage Cluster Sampling Probability Proportion to Size (PPS) from random walk if the population is concentrated within clusters and clusters tend to be found in the same	region or geographic area.

  After selecting clusters based on their population size, enumerators survey every nth household until the required number of households is selected.

  For example, enumerators may visit every third household within the cluster."

  ppsframe <- "Multiple-Stage Cluster Sampling with Probability Proportion to Size (PPS) from sample frame.
  After selecting clusters based on their population size, Use the sample frame created from the listing exercise to randomly select households."

  srsstrata <- "Simple Random Sampling (SRS) within Strata.
  SRS gives each group within a stratum an equal chance of being selected for the sample."


  acs <- "Adaptive Cluster Sampling  method first divides the population into a grid of plots and randomly selects a few initial plots.
  The selected plots are reviewed to determine whether the population is present or not – a condition required to select the plot.

  If there are a minimum number of households of the population group present in the plot, the research team also reviews any neighbouring plots to ensure the minimum number of households of the population group are present.

  Each plot that has the minimum number of households of the population group is selected until there are no additional neighbouring plots with such households present.

  Clusters of plots are then generated where the survey will take place.

  This method may be most useful when the population is very concentrated in certain plots, but these plots are widely dispersed."

  lts <- " Time Location Sampling. Identify times and locations where the population is likely to gather and select the times and locations where the greatest number will be present.
  Conduct interviews on location."

  rds <- "Use Respondent Driven Sampling (RDS). Identify initial ‘seed’ population who are well connected in their community and can refer additional households of the same population group to the survey.

  Respondents will continue to recruit additional households until the desired sample size is reached.

  Additional data protection considerations need to be taken when adopting RDS as seeds will be sharing personal data of other potential survey respondents.

  Carryig out a Respondent Driven Sampling (RDS) approach, entails conducting a formative survey to identify seeds and prepare RDS coupons"


  ## reactive value for that sub module...  ####
  reactLocal <- reactiveValues(
    availablereg = NULL  ,
    lessthan5000 = NULL  ,
    traceable = NULL  ,
    spread = NULL  ,
    strata = NULL  ,
    budget = NULL  ,
    gather = NULL  ,
    network = NULL  ,
    expert = NULL  ,
    sampling = "Answe Screening questions"  ,
    ## conditional display
    show_lessthan5000 = FALSE  ,
    show_traceable= FALSE  ,
    show_spread= FALSE  ,
    show_strata= FALSE  ,
    show_budget= FALSE  ,
    show_gather = FALSE  ,
    show_network= FALSE  ,
    show_expert= FALSE
  )

  ## Get input into local reactive value

  # availablereg ####
  observeEvent(eventExpr = input$availablereg, {
    reactLocal$availablereg <- input$availablereg

    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")

    if( reactLocal$availablereg == TRUE) {
      reactLocal$show_lessthan5000 <- TRUE
    }
  })

  # lessthan5000 ####
  observeEvent(eventExpr = input$lessthan5000, {
    reactLocal$lessthan5000 <- input$lessthan5000

    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")


    if( reactLocal$availablereg == TRUE) {
      reactLocal$show_traceable <- TRUE
      reactLocal$show_spread <- TRUE
      # reactLocal$show_strata <- TRUE
      # reactLocal$show_budget <- TRUE
      # reactLocal$show_gather <- TRUE
      # reactLocal$show_network <- TRUE
      # reactLocal$show_expert <- TRUE
    }
  })
  observeEvent(reactLocal$show_lessthan5000, {
    if(isTRUE(reactLocal$show_lessthan5000)) {
      golem::invoke_js("show", paste0("#", ns("lessthan5000")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("lessthan5000")))
    }
  })

  # traceable ####
  observeEvent(eventExpr = input$traceable, {
    reactLocal$traceable <- input$traceable
    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")

  })
  observeEvent(reactLocal$show_traceable, {
    if(isTRUE(reactLocal$show_traceable)) {
      golem::invoke_js("show", paste0("#", ns("traceable")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("traceable")))
    }
  })

  # spread ####
  observeEvent(eventExpr = input$spread, {
    reactLocal$spread <- input$spread
    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")
  })
  observeEvent(reactLocal$show_spread, {
    if(isTRUE(reactLocal$show_spread)) {
      golem::invoke_js("show", paste0("#", ns("spread")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("spread")))
    }
  })

  # strata ####
  observeEvent(eventExpr = input$strata, {
    reactLocal$strata <- input$strata
    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")
  })
  observeEvent(reactLocal$show_strata, {
    if(isTRUE(reactLocal$show_strata)) {
      golem::invoke_js("show", paste0("#", ns("strata")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("strata")))
    }
  })

  # budget ####
  observeEvent(eventExpr = input$budget, {
    reactLocal$budget <- input$budget
    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")
  })
  observeEvent(reactLocal$show_budget, {
    if(isTRUE(reactLocal$show_budget)) {
      golem::invoke_js("show", paste0("#", ns("budget")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("budget")))
    }
  })

  # gather ####
  observeEvent(eventExpr = input$gather, {
    reactLocal$gather <- input$gather
    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")
  })
  observeEvent(reactLocal$show_gather, {
    if(isTRUE(reactLocal$show_gather)) {
      golem::invoke_js("show", paste0("#", ns("gather")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("gather")))
    }
  })

  # network ####
  observeEvent(eventExpr = input$network, {
    reactLocal$network <- input$network
    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")
  })
  observeEvent(reactLocal$show_network, {
    if(isTRUE(reactLocal$show_network)) {
      golem::invoke_js("show", paste0("#", ns("network")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("network")))
    }
  })

  # expert ####
  observeEvent(eventExpr = input$expert, {
    reactLocal$expert <- input$expert
    reactLocal$sampling <- dplyr::case_when(
      reactLocal$availablereg == FALSE & reactLocal$traceable %in% c("unknown" ) ~ nonprob,
      reactLocal$availablereg == TRUE & reactLocal$spread %in% c("concentrated") ~ srs,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("concentrated") == TRUE ~ ppswalk,
      reactLocal$traceable %in% c("partknown", "known") ~ ppsframe,
      reactLocal$strata == TRUE ~ srsstrata,
      reactLocal$budget == TRUE & reactLocal$spread %in% c("scattered") ~ acs,
      reactLocal$gather == TRUE ~ lts,
      reactLocal$expert == TRUE ~ rds,
      TRUE ~ "We are not yet able to define the adequate sampling approach")
  })
  observeEvent(reactLocal$show_expert, {
    if(isTRUE(reactLocal$show_expert)) {
      golem::invoke_js("show", paste0("#", ns("expert")))
    } else {
      golem::invoke_js("hide", paste0("#", ns("expert")))
    }
  })

  ### Adequate sampling options based on decision tree #####
  output$sampling <- renderText({
    ## Display it on the GUI
    reactLocal$sampling
    ## get this recorded in the main app reactive value
    # AppReactiveValue$sampling <- list( reactLocal$sampling,
    #                                    thisgroup)

    })
}

## copy to body.R
# mod_screener_ui("screener_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "screener",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_screener_server, "screener_ui_1", AppReactiveValue)

