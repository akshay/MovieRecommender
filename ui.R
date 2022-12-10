## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),

          dashboardSidebar(
            sidebarMenu(
              menuItem("Popular Movies", tabName = "popular", icon = icon("dashboard")),
              menuItem("Recommended Movies", tabName = "recommended", icon = icon("th"))
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
            tabItems(
              tabItem(tabName = "popular",
                h2("Popular Movies"),
                fluidRow(
                  box(width = 11, title = "Select genre to filter to", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      selectInput("genre", "Genre:", c("Action", "Adventure", "Animation",
                                                       "Children's", "Comedy", "Crime",
                                                       "Documentary", "Drama", "Fantasy",
                                                       "Film-Noir", "Horror", "Musical",
                                                       "Mystery", "Romance", "Sci-Fi",
                                                       "Thriller", "War", "Western"))
                  )
                ),
                fluidRow(
                  box(width = 11, title = "Highest rated movies with 1000+ ratings", status = "primary", solidHeader = TRUE, collapsible = FALSE,
                      div(class = "rateitems",
                          uiOutput('popular')
                      )
                  )
                )
              ),
              tabItem(tabName = "recommended",
                h2("Recommended Movies"),
                fluidRow(
                  box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      div(class = "rateitems",
                          uiOutput('ratings')
                      )
                  )
                ),
                fluidRow(
                  useShinyjs(),
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("results")
                  )
                )
              )
            )
          )
    )
)
