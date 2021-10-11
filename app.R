library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(dplyr)
library(ffscrapr)
library(DT)
library(waiter)

ui <- dashboardPage(
  dark = NULL,
  header = dashboardHeader(
    h4("ESPN Potential Points Calculator", style = "color:#fff;"),
    status = "danger",
    skin = "dark"
  ),
  sidebar = dashboardSidebar(
    disable = TRUE
  ),
  body = dashboardBody(
    waiter::useWaiter(),
    waiter::waiterOnBusy(html = waiter::spin_dots(),color = waiter::transparent(0.5)),
    box(
      title = "Inputs",
      width = 4,
      status = "gray-dark",
      solidHeader = TRUE,
      fluidRow(
        column(
          width = 12,
          pickerInput("season",
                      label = "Season",
                      choices = 2021:2018,
                      selected = 2021),
          textInput("league_id",
                    label = "League ID"),
          tags$details(
            tags$summary("Add Authentication Cookies"),
            textInput("swid",
                      label = "SWID"),
            textInput("espn_s2",
                      label = "ESPN S2"),
            markdown("You can find the values for cookies by following these [instructions](https://ffscrapr.ffverse.com/articles/espn_authentication.html).")
          )
        )
      ),
      footer = div(actionButton("load", label = "Calculate!", status = "success"),style = "text-align:center;")
    ),
    uiOutput("potential_points")
  )
)

server <- function(input, output, session) {

  potential_points <- reactiveVal()

  observeEvent(input$load,{

    req(input$season)
    req(input$league_id)

    conn <- espn_connect(input$season,input$league_id, swid = input$swid, espn_s2 = input$espn_s2)
    # conn <- espn_connect(season = 2021, league_id = 1178049)

    player_week <- espn_potentialpoints(conn)

    week <- player_week %>%
      group_by(week, franchise_id, franchise_name, franchise_score) %>%
      summarise(
        optimal_score = sum(player_score * !is.na(optimal_slot), na.rm = TRUE),
      ) %>%
      ungroup() %>%
      mutate(efficiency = round(franchise_score/optimal_score,3))

    season <- week %>%
      group_by(franchise_id, franchise_name) %>%
      summarise(
        franchise_score = sum(franchise_score, na.rm = TRUE),
        optimal_score = sum(optimal_score, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(efficiency =  round(franchise_score/optimal_score,3))

    list(
      week_details = player_week,
      week_summary = week,
      season_summary = season
    ) %>%
      potential_points()
  })

  fn_tabPanel <- function(x,x_name){
    tabPanel(title = x_name,
             DT::datatable(x,
                           rownames = FALSE,
                           class = "compact stripe nowrap",
                           filter = "top",
                           options = list(scrollX = TRUE)
             )
             # tantastic::fmt_dtcol(x,c("franchise_score","optimal_score","efficiency"))
    )
  }

  output$potential_points <- renderUI({
    req(potential_points())

    player_week <- fn_tabPanel(potential_points()$week_details,"Week Details")
    week <- fn_tabPanel(potential_points()$week_summary,"Week Summary")
    season <- fn_tabPanel(potential_points()$season_summary,"Season Summary")

    box(
      title = "Potential Points",
      side = "right",
      width = 12,
      status = "gray-dark",
      solidHeader = TRUE,
      tabBox(
        type = "pills",
        width = 12,
        player_week,
        week,
        season
      ),
      footer = div(downloadButton("download", status = "btn-success"), style = "text-align:center;")
    )

  })

  output$download <- downloadHandler(
    filename = "ESPN_PP.xlsx",
    content = function(file){
      writexl::write_xlsx(potential_points(),path = file,format_headers = TRUE)
    }
  )


}

shinyApp(ui, server)
