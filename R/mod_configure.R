#' Module UI

#' @title mod_configure_ui and mod_configure_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
#' @import refugees
#' @importFrom countrycode countrycode
#' @importFrom dplyr across mutate filter select arrange pull
#' @importFrom purrr set_names
#' @keywords internal

mod_configure_ui <- function(id) {
	ns <- NS(id)
	tabItem(
		tabName = "configure",

		fluidRow(
		  column(
		    width = 12,
		    h2('Configure the sampling assistant!'),
		    p("Sampling describes the selection of a subset of individuals or households from a population of
interest in a controlled way that allows us to make generalisations about the characteristics of that
population without interviewing everyone."),
		    p("Before embarking on your Result Monitoring Survey, it is therefore essential
		    to have a complete and sound Means of Verification (MoV) table that clearly
		      defines not only the indicators, but also the population groups for which survey-based data is required.")
		  )
		),

		fluidRow(
		  shinydashboard::box(
		    title = "Operation Context",
		    #  status = "primary",
		    status = "info",
		    solidHeader = FALSE,
		    collapsible = TRUE,
		    #background = "light-blue",
		    width = 12,
		    fluidRow(
		      selectizeInput(inputId = ns("country"),
		                     label = " Please, select your Country",
		                     width =  "500px",
		                     choices = refugees::population |>
		                       dplyr::filter(year == 2022  ) |>
		                       dplyr::mutate(
		                         tot = rowSums(
		                           dplyr::across(c("refugees","asylum_seekers","oip",
		                                           "returned_refugees","returned_idps",
		                                           "stateless",
		                                           "idps",
		                                           "ooc")), na.rm=TRUE) ) |>
		                       dplyr::summarise(tot = sum(tot, na.rm = TRUE) , .by = coa)  |>
		                       dplyr::filter( tot > 2000) |>
		                       dplyr::arrange(coa) |>
		                       dplyr::pull(coa) |>
		                       purrr::set_names(
		                         refugees::population |>
		                           dplyr::filter(year == 2022  ) |>
		                           dplyr::mutate(
		                             tot = rowSums(
		                               dplyr::across(c("refugees","asylum_seekers","oip",
		                                               "returned_refugees","returned_idps",
		                                               "stateless",
		                                               "idps",
		                                               "ooc")), na.rm=TRUE) ) |>
		                           dplyr::summarise(tot = sum(tot, na.rm = TRUE) , .by = coa)  |>
		                           dplyr::filter( tot > 2000) |>
		                           dplyr::arrange(coa) |>
		                           dplyr::pull(coa)   |>
		                           countrycode::countrycode( origin = 'unhcr', destination = 'country.name')),
		                     selected = "PAN",
		                     multiple = FALSE,
		                     options = NULL)
		    ),
		    fluidRow(

		      column(
		        width = 8,
		        plotOutput(outputId = ns("universe"),
		                   width = "100%",
		                   height = "500px")
		      ),
		      column(
		        width = 4,
		        shinydashboard::box(
		          title = "Based on your Indicators Means of Verification table...",
		          #  status = "primary",
		          status = "info",
		          solidHeader = FALSE,
		          collapsible = TRUE,
		          #background = "light-blue",
		          width = 12,
		        checkboxGroupInput(  inputId = ns("poptype"),
		          label = "Select the Population group(s) you need to sample
		          for the Result Monitoring Survey ")
		        ),

		        shinydashboard::box(
		          title = "What data collection mode will you implement? ",
		          #  status = "primary",
		          status = "info",
		          solidHeader = FALSE,
		          collapsible = TRUE,
		          #background = "light-blue",
		          width = 12,
      		        div(
      		          id = ns("targetm_ras"),
      		          selectInput(
      		            inputId = ns("ras_mode"),
      		            label = "Refugee and Asylum Seekes",
      		            choices =  c(  "Telephone Interview - CATI " = "CATI",
      		                           "Face to Face inteview - CAPI " = "CAPI",
      		                           "Mixed of CAPI & CATI " = "mixed" )
      		          )
      		        ),
		              div(
      		          id = ns("targetm_sta"),
      		          selectInput(
      		            inputId = ns("sta_mode"),
      		            label = "Stateless",
      		            choices =  c(  "Telephone Interview - CATI " = "CATI",
      		                           "Face to Face inteview - CAPI " = "CAPI",
      		                           "Mixed of CAPI & CATI " = "mixed" )
      		          )
      		        ),
		              div(
      		          id = ns("targetm_ret"),
      		          selectInput(
      		            inputId = ns("ret_mode"),
      		            label = "Returnees",
      		            choices =  c( "Telephone Interview - CATI " = "CATI",
      		                          "Face to Face inteview - CAPI " = "CAPI",
      		                           "Mixed of CAPI & CATI " = "mixed" )
      		          )
      		        ),
		              div(
      		          id = ns("targetm_idp"),
      		          selectInput(
      		            inputId = ns("idp_mode"),
      		            label = "Internally Displaced Persons",
      		            choices =  c(   "Telephone Interview - CATI " = "CATI",
      		                            "Face to Face inteview - CAPI " = "CAPI",
      		                           "Mixed of CAPI & CATI " = "mixed"  )
      		          )
      		        ),
		              div(
      		          id = ns("targetm_ooc"),
      		          selectInput(
      		          #radioButtons(
      		            inputId = ns("ooc_mode"),
      		            label = "Other People with and for whom UNHCR works",
      		            choices =  c(  "Telephone Interview - CATI " = "CATI",
      		                           "Face to Face inteview - CAPI " = "CAPI",
      		                           "Mixed of CAPI & CATI " = "mixed" )
      		          )
      		        )
		        ),



		      )
		    )

		  )
		)
	)
}

#' Module Server
#' @noRd
#' @import shiny
#' @import tidyverse
#' @import refugees
#' @import unhcrthemes
#' @keywords internal

mod_configure_server <- function(input, output, session, AppReactiveValue) {
	ns <- session$ns

	observeEvent(eventExpr = input$country,{
	  AppReactiveValue$country <- input$country

	  AppReactiveValue$group <- as.data.frame( refugees::population |>
	    dplyr::filter(year == 2022 &
	                    coa ==  input$country ) |>
	    dplyr::mutate(
	      ras = rowSums(
	        dplyr::across(c("refugees","asylum_seekers","oip")), na.rm=TRUE), #refugees + asylum_seekers + oip,
	      ret = rowSums(
	        dplyr::across(c("returned_refugees","returned_idps")), na.rm=TRUE) ) |> #returned_refugees + returned_idps)
	    dplyr::summarise(RAS = sum(ras, na.rm = TRUE),
	                     STA = sum(stateless, na.rm = TRUE),
	                     RET = sum(ret, na.rm = TRUE),
	                     IDP = sum(idps, na.rm = TRUE),
	                     OOC = sum(ooc, na.rm = TRUE), .by = coa)  |>
	    tidyr::pivot_longer(cols =  c(RAS, STA, RET, IDP, OOC )) |>
	    dplyr::mutate(name2 = dplyr::recode( name,
	                                         "RAS" = "Pillar-1- Refugee, Asylum Seeker & Other in Need of International Protection" ,
	                                         "STA" = "Pillar-2- Stateless",
	                                         "RET" = "Pillar-3- Returnees",
	                                         "IDP" = "Pillar-4- Internally Displaced Persons",
	                                         "OOC" = "Other People with and for whom UNHCR works"
	    )  ) |>
	    dplyr::mutate(name2 = factor(name2, levels = c(
	      "Other People with and for whom UNHCR works",
	      "Pillar-4- Internally Displaced Persons",
	      "Pillar-3- Returnees" ,
	      "Pillar-2- Stateless",
	      "Pillar-1- Refugee, Asylum Seeker & Other in Need of International Protection")))  )


	  ## get a chart to summarise
	  AppReactiveValue$universe <-
	    ggplot2::ggplot( data = AppReactiveValue$group,
	                     ggplot2::aes(value,  name2 )) +
	    ggplot2::geom_col(fill = unhcrthemes::unhcr_pal(n = 1, "pal_blue"),
	                      width = 0.8) +
	    ## Position label differently in the bar in white - outside bar in black
	    ggplot2::geom_text(
	      data = subset(AppReactiveValue$group, value < max(value) / 1.5),
	      ggplot2::aes( x = value, y = name2,
	        label =   scales::label_comma()(value)),
	      hjust = -0.1 ,  vjust = 0.5,  colour = "black", size = 6	    ) +
	    ggplot2::geom_text(
	      data = subset(AppReactiveValue$group, value >= max(value) / 1.5),
	      ggplot2::aes( x = value, y = name2,
	        label =   scales::label_comma()(value)),
	      hjust = 1.1 , vjust = 0.5,  colour = "white"   ) +
	    ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
	    ggplot2::scale_x_continuous(expand =
	                                  ggplot2::expansion(mult = c(0, 0.1))) +
	    ggplot2::labs(title = "Potential Sampling Universe for Result Monitoring Survey",
	                  subtitle = paste0( countrycode::countrycode(
	                    AppReactiveValue$country,
	                    origin = 'unhcr',
	                    destination = 'country.name')
	                    , " | 2022"),
	                  caption = "Source: UNHCR Refugee Data Finder") +
	    unhcrthemes::theme_unhcr(font_size = 20, rel_small = 6/9,
	                             grid = FALSE,axis = FALSE,
	                             axis_title = FALSE, axis_text = "y")


	  AppReactiveValue$group2 <- AppReactiveValue$group   |>
	    dplyr::filter(value > 0)

	  AppReactiveValue$groupName <- AppReactiveValue$group2  |>
	    dplyr::pull(name) |>
	    purrr::set_names(AppReactiveValue$group2 |>
	                       dplyr::mutate(name3 =paste0( name2," (" ,
	                                                    scales::label_number(accuracy = 1,
	                                                                         scale_cut = scales::cut_short_scale())(value),
	                                                    # value,
	                                                    " ind.)" )) |>
	                       dplyr::pull(name3) )

	  ## update dropdown
	  updateCheckboxGroupInput(session,
	                           "poptype",
	                           choices = AppReactiveValue$groupName  )

	})

#	observeEvent(eventExpr = input$AppReactiveValue$targetm_ras1, {


	observeEvent(eventExpr = input$poptype, {
	  AppReactiveValue$poptype <- input$poptype

	  if ( "RAS" %in% input$poptype ) {
	     AppReactiveValue$ras_universe <- AppReactiveValue$group |>
	         dplyr::filter( name == "RAS") |>
	         dplyr::pull(value)

	     if(AppReactiveValue$ras_universe < 5000) {
	       AppReactiveValue$ras_islessthan5000 <- TRUE
	       }

	     AppReactiveValue$targetm_ras1 <- TRUE
	  }

	  if(isTRUE(AppReactiveValue$targetm_ras1)){
	    golem::invoke_js("show", paste0("#", ns("targetm_ras")))
	  }  else {
	    golem::invoke_js("hide", paste0("#", ns("targetm_ras")))  }


	  if ( "STA" %in% input$poptype ) {
	    AppReactiveValue$sta_universe <- AppReactiveValue$group |>
	      dplyr::filter( name == "STA") |>
	      dplyr::pull(value)

	    if(AppReactiveValue$sta_universe < 5000) {
	      AppReactiveValue$sta_islessthan5000 <- TRUE
	    }

	    AppReactiveValue$targetm_sta1 <- TRUE
	  }

	  if(isTRUE(AppReactiveValue$targetm_sta1)){
	    golem::invoke_js("show", paste0("#", ns("targetm_sta")))
	  }  else {
	    golem::invoke_js("hide", paste0("#", ns("targetm_sta")))  }

	  if ( "RET" %in% input$poptype ) {
	    AppReactiveValue$ret_universe <- AppReactiveValue$group |>
	      dplyr::filter( name == "RET") |>
	      dplyr::pull(value)

	    if(AppReactiveValue$ret_universe < 5000) {
	      AppReactiveValue$ret_islessthan5000 <- TRUE
	    }

	    AppReactiveValue$targetm_ret1 <- TRUE
	  }

	  if(isTRUE(AppReactiveValue$targetm_ret1)){
	    golem::invoke_js("show", paste0("#", ns("targetm_ret")))
	  }  else {
	    golem::invoke_js("hide", paste0("#", ns("targetm_ret")))  }

	  if ( "IDP" %in% input$poptype ) {
	    AppReactiveValue$idp_universe <- AppReactiveValue$group |>
	      dplyr::filter( name == "IDP") |>
	      dplyr::pull(value)

	    if(AppReactiveValue$idp_universe < 5000) {
	      AppReactiveValue$idp_islessthan5000 <- TRUE
	    }

	    AppReactiveValue$targetm_idp1 <- TRUE
	  }

	  if(isTRUE(AppReactiveValue$targetm_idp1)){
	    golem::invoke_js("show", paste0("#", ns("targetm_idp")))
	  }  else {
	    golem::invoke_js("hide", paste0("#", ns("targetm_idp")))  }

	  if ( "OOC" %in% input$poptype ) {
	    AppReactiveValue$ooc_universe <- AppReactiveValue$group |>
	      dplyr::filter( name == "OOC") |>
	      dplyr::pull(value)

	    if(AppReactiveValue$ooc_universe < 5000) {
	      AppReactiveValue$ooc_islessthan5000 <- TRUE
	    }

	    AppReactiveValue$targetm_ooc1 <- TRUE
	  }

	  if(isTRUE(AppReactiveValue$targetm_ooc1)){
	    golem::invoke_js("show", paste0("#", ns("targetm_ooc")))
	  }  else {
	    golem::invoke_js("hide", paste0("#", ns("targetm_ooc")))  }


	  lab <- data.frame(
	    pop = c("RAS" ,
	             "STA" ,
	             "RET" ,
	             "IDP" ,
	             "OOC"),
	    lab = c(
	     "Pillar-1- Refugee, Asylum Seeker & Other in Need of International Protection" ,
	     "Pillar-2- Stateless",
	     "Pillar-3- Returnees",
	     "Pillar-4- Internally Displaced Persons",
	     "Other People with and for whom UNHCR works")
	  )

	 AppReactiveValue$poptypefilt <- data.frame( pop = AppReactiveValue$poptype) |>
	    dplyr::left_join(lab, by = c("pop") )

	  #browser()

	})


	observeEvent(eventExpr = input$ras_mode, {
	  AppReactiveValue$ras_mode <- input$ras_mode
	})
	observeEvent(eventExpr = input$sta_mode, {
	  AppReactiveValue$sta_mode <- input$sta_mode
	})
	observeEvent(eventExpr = input$ret_mode, {
	  AppReactiveValue$ret_mode <- input$ret_mode
	})
	observeEvent(eventExpr = input$idp_mode, {
	  AppReactiveValue$idp_mode <- input$idp_mode
	})
	observeEvent(eventExpr = input$ooc_mode, {
	  AppReactiveValue$ooc_mode <- input$ooc_mode
	})

	output$universe <- renderPlot({
	  AppReactiveValue$universe
	})

}

## copy to body.R
# mod_configure_ui("configure_ui_1")

## copy to sidebar.R
# menuItem("displayName",tabName = "configure",icon = icon("user"))

## and copy to app_server.R
# callModule(mod_configure_server, "configure_ui_1", AppReactiveValue)

