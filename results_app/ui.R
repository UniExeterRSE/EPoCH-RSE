library(shiny)
library(DT)
library(tidyverse)
library(bslib)

ui <- function(request) {
        fluidPage(theme = bs_theme(version = 4, bootswatch = "minty"),
                  id = 'epoch',
                  # App Title
                  titlePanel(tags$b("Epoch", style = "font-size: 110%, font-family:Helvetica; color:#010151"), windowTitle = "Epoch"),
                  hr(),
                  # App Description
                  p("Short overview of project", style = "font-size: 90%"), 
                  hr(),
        # User Input Section -------------------------------
        sidebarLayout(
                sidebarPanel(
                        selectizeInput(inputId = "input 1", 
                                       label = tags$h4("input 1:"),
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = T,
                                       options = list(placeholder = '----------', maxItems = 1)),
                        hr(),
                        selectizeInput(inputId = "input 2", 
                                       label = tags$h4("input 2:"),
                                       choices = NULL,
                                       selected = NULL,
                                       multiple = T,
                                       options = list(placeholder = '----------', maxItems = 1)),
                        hr(),
                        br()),
                # Output of Plot, Data, and Summary -------------------------------
                mainPanel(
                        tabsetPanel(id = 'main_tabs',
                                tabPanel("Plot by exposure", icon = icon("chart-scatter")#, 
                                         #tags$br(), plotOutput("figure_rsra", height = 625),
                                         #tags$style(type='text/css', "#fig_link_rsra {
                                         #           font-family: 'Arial';
                                         #           font-size: 65%;}")
                                         #conditionalPanel(condition = "output.figure_rsra",
                                         #                       hr(), uiOutput(outputId = 'fig_link_rsra'), tags$br(),
                                         #                       downloadButton(outputId = 'dl_fig_rsra', 'Download plot as .png', class = 'btn-default btn-sm')
                                         ),
                                tabPanel("Plot by outcome", icon = icon("chart-scatter")#, 
                                         #tags$br(), plotOutput("figure_rdiff", height = 625),
                                         #tags$style(type='text/css', "#fig_link_rdiff {
                                         #           font-family: 'Arial';
                                         #           font-size: 65%;}")
                                         #conditionalPanel(condition = "output.figure_rdiff",
                                         #                       hr(), uiOutput(outputId = 'fig_link_rdiff'), tags$br(),
                                         #                       downloadButton(outputId = 'dl_fig_rdiff', 'Download plot as .png', class = 'btn-default btn-sm')
                                         ),

                        )
                )
        ),
        # Footer -------------------------------
        tags$br(),
        hr(),
        p('Footer text', style = "font-size: 85%")
)
}