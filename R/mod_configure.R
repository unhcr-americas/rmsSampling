#' Module UI

#' @title mod_configure_ui and mod_configure_server
#' @description A shiny module.
#' @description A shiny module.
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinydashboard
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
		      defines not only the indicators, but also the popuation group for which survey-based data is required.")
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
		      column(
		        width = 4,
  		      selectizeInput(inputId = ns("country"),
		                     label = " Select Country",
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
		                     options = NULL),
		        hr(),
		        checkboxGroupInput(  inputId = ns("poptype"),
		                           label = "Based on your Means of Verification (MoV) table
		                           and secondary data review, select the Population group you need to sample for a specific survey ",
		                           choices = c(
		                             "Pillar-1- Refugee, Asylum Seeker & Other in Need of International Protection" ="RAS",
		                             "Pillar-2- Stateless"="STA",
		                             "Pillar-3- Returnees"= "RET",
		                             "Pillar-4- Internally Displaced Persons"=  "IDP",
		                             "Other People with and for whom UNHCR works"= "OOC" ),
		                           selected = c("RAS" ) # "RET",  "STA",  "IDP", "OOC"
		      )
		      ),
		      column(
		        width = 8,
		        actionButton(inputId = ns("showplot"),
		                     label = "Display the sampling niverse"),
		        hr(),
		        plotOutput(outputId = ns("universe"),
		                   width = "100%",
		                   height = "550px")
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
#' @keywords internal

mod_configure_server <- function(input, output, session, AppReactiveValue) {
	ns <- session$ns

	observeEvent(eventExpr = input$country,{
	  AppReactiveValue$country <- input$country
	})

	observeEvent(eventExpr = input$poptype, {
	  AppReactiveValue$poptype <- input$poptype

	  ## Based on selection show or hid screening questions..
	   if( any(AppReactiveValue$poptype %in% c("RAS"))) {
	      AppReactiveValue$show_ras <- TRUE
	    } else  if( any( !(AppReactiveValue$poptype %in% c("RAS"))))  {
	      AppReactiveValue$show_ras <- FALSE
	    }

	  if( any(AppReactiveValue$poptype %in% c("STA")))  {
	    AppReactiveValue$show_sta <- TRUE
	  } else if( any(!(AppReactiveValue$poptype %in% c("STA"))))  {
	    AppReactiveValue$show_sta <- FALSE
	  }

	  if( any(AppReactiveValue$poptype %in% c("RET")))  {
	    AppReactiveValue$show_ret <- TRUE
	  } else if( any(!(AppReactiveValue$poptype %in% c("RET")) ))  {
	    AppReactiveValue$show_ret <- FALSE
	  }

	  if( any(AppReactiveValue$poptype %in% c("IDP")))  {
	    AppReactiveValue$show_ras <- TRUE
	  } else  if( any(! (AppReactiveValue$poptype %in% c("IDP"))) ) {
	    AppReactiveValue$show_ras <- FALSE
	  }

	  if( any(AppReactiveValue$poptype %in% c("OOC")))  {
	    AppReactiveValue$show_ras <- TRUE
	  } else if( any(!(AppReactiveValue$poptype %in% c("OOC")) ))  {
	    AppReactiveValue$show_ras <- FALSE
	  }

	})

	observeEvent(eventExpr = input$showplot, {
	  # validate(
	  #    need(isTruthy(AppReactiveValue$poptype),
	  #         message = "Please select at least one population type")
	  #  )

	  univ <-
	  refugees::population |>
	  dplyr::filter(year == 2022 &
	                  coa ==  AppReactiveValue$country ) |>
	  #  coa ==  "PAN" ) |>
	  dplyr::mutate(
	    ras = rowSums(
	      dplyr::across(c("refugees","asylum_seekers","oip")), na.rm=TRUE), #refugees + asylum_seekers + oip,
	    ret = rowSums(
	      dplyr::across(c("returned_refugees","returned_idps")), na.rm=TRUE) ) |> #returned_refugees + returned_idps)
	  dplyr::summarise(RAS = sum(ras, na.rm = TRUE),
	                   STA = sum(stateless, na.rm = TRUE),
	                   RET = sum(ret, na.rm = TRUE),
	                   IDP = sum(idps, na.rm = TRUE),
	                   OOC = sum(ooc, na.rm = TRUE), .by = coa)

	  ## inject in appreactive value the universe to assess then the sample

	  ## get a chart to summarise
	  AppReactiveValue$universe <- univ |>
	  tidyr::pivot_longer(cols =  c(RAS, STA, RET, IDP, OOC )) |>
	  dplyr::mutate(name2 = dplyr::recode( name,
	                                       "RAS" = "Pillar-1- Refugee, Asylum Seeker & Other in Need of International Protection" ,
	                                       "STA" = "Pillar-2- Stateless",
	                                       "RET" = "Pillar-3- Returnees",
	                                       "IDP" = "Pillar-4- Internally Displaced Persons",
	                                       "OOC" = "Other People with and for whom UNHCR works"
	  )  ) |>
	  ggplot2::ggplot(
	    ggplot2::aes(value, reorder(name2, value))) +
	  ggplot2::geom_col(fill = unhcrthemes::unhcr_pal(n = 1, "pal_blue"),
	                    width = 0.8) +
	  ggplot2::geom_text(
	    ggplot2::aes(label = scales::label_comma()(value)),
	    hjust = -0.2) +
	  ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 30)) +
	  ggplot2::scale_x_continuous(expand =
	                                ggplot2::expansion(mult = c(0, 0.1))) +
	  ggplot2::labs(title = "Sampling Universe",
	                subtitle = paste0( AppReactiveValue$country, " | 2022"),
	                caption = "Source: UNHCR Refugee Data Finder") +
	  unhcrthemes::theme_unhcr(font_size = 18,
	                           grid = FALSE,
	                           axis = FALSE,
	                           axis_title = FALSE,
	                           axis_text = "y")

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

