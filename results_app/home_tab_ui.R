tabPanel(title="Home",value="tab1",
         fluidRow(
           column(width = 3,
             actionButton("load_results","Click to load results"),
           ),
           fluidRow(
             column(width = 3,
              wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 720px;",
                                          plotOutput("par_yr", height = 680, click = "plot_click"))
             )
           )
         )
)