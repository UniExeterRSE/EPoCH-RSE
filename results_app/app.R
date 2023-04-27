
# Load required packages -------------------------------
library(shiny)
library(tidyverse)
library(plotly)
library(shinycssloaders)
library(shinyjs)
library(shinyTree)
library(bslib)

source("data.R")
source("plot.R")

# Hard coded choices for comparisons
exposures <- list("All", "Smoking", "Alcohol consumption", "Caffeine consumption", "Socioeconomic position", "Physical activity", "Diet")
outcomes <- list("All", "Perinatal survival", "Immunological", "Body size and composition", "Psychosocial and cognitive", "Serum biomarkers", "Negative control outcomes", "Blood pressure")
models <- list("Model 1a", "Model 1b")

ui <- function(request) {
        fluidPage(title = "EPoCH data analysis tool", id = 'epoch',
                  theme = bs_theme(version = 4, bootswatch = "minty"),#, heading_font = font_google("Segoe UI")),
                  hr(),
        # User Input Section -------------------------------
        sidebarLayout(
                sidebarPanel( width = 3,
                        # Logo
                        tags$img(src="https://cpb-eu-w2.wpmucdn.com/blogs.bristol.ac.uk/dist/c/500/files/2018/11/Untitled-26hzp4l.png",
                                      width = 180, height = 50, style="float:left; margin-left: 10px; margin-right: 15px; margin-top: 2px; margin-bottom: 30px", label = tags$h4("logo")),
                        actionButton("load_results","Click to load results"),
                        hr(),
                        selectizeInput(inputId = "exposure_choice",
                                       label = tags$h4("Exposure type:"),
                                       choices = exposures,
                                       selected = NULL,
                                       multiple = T,
                                       options = list(placeholder = '----------', maxItems = 1)),
                        selectizeInput(inputId = "outcome_choice",
                                       label = tags$h4("Outcome:"),
                                       choices = outcomes,
                                       selected = NULL,
                                       multiple = T,
                                       options = list(placeholder = '----------', maxItems = 1)),
                        selectizeInput(inputId = "model_choice",
                                       label = tags$h4("Model:"),
                                       choices = models,
                                       selected = NULL,
                                       multiple = T,
                                       options = list(placeholder = '----------', maxItems = 1)),
                        hr(),
                        actionButton("plot_data","Visualise data"),
                        #hr(),
                        #selectizeInput(inputId = "future_loader",
                        #               label = tags$h4("Placeholder for future data loader:"),
                        #               choices = NULL,
                        #               selected = NULL,
                        #               multiple = T,
                        #               options = list(placeholder = '----------', maxItems = 1)),
                        hr(),
                        p('Footer text', style = "font-size: 85%"),
                        br()),
                # Output of Plot, Data, and Summary -------------------------------
                mainPanel( width = 9,
                        tabsetPanel(id = 'main_tabs',
                                tabPanel("Plot by exposure", icon = icon("chart-simple"),
                                         tabsetPanel(
                                                tabPanel("Manhattan", icon = icon("chart-simple"),
                                                    textOutput('Text1'),
                                                    plotlyOutput("exposureManhattanPlot")
                                                        ),
                                                tabPanel("Volcano",
                                                    plotlyOutput("exposureVolcanoPlot")
                                                        )
                                                  )
                                         ),
                                tabPanel("Plot by Outcome", icon = icon("chart-simple"),
                                         tabsetPanel(
                                                tabPanel("Manhattan",
                                                    textOutput('Text2'),
                                                    plotlyOutput("outcomeManhattanPlot")
                                                        ),
                                                tabPanel("Volcano",
                                                    plotlyOutput("outcomeVolcanoPlot")
                                                        )
                                                  )
                                         ),
                                tabPanel("Plot by exposure coefficient", icon = icon("chart-simple")),
                                tabPanel("Forest plots", icon = icon("chart-simple")),
                                tabPanel("Saved plots", icon = icon("save"))
                                    )
                            )
                      ),
        # Footer -------------------------------
        #tags$br(),
        #hr(),
        #p('Footer text', style = "font-size: 85%")
)
}

server <- function(input, output, session) {

  global_data <- reactiveValues(data = NULL, active_models = NULL, active_exp = NULL)

  source("data_server.R",local=T)$value
  source("plot_server.R",local=T)$value
  #source("tab2_server_exposures.R",local=T)$value 
}


# Load UI and server controls
shinyApp(ui = ui, server = server)