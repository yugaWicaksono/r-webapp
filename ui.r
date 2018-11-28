library(shiny)
library(shinythemes)
library(ade4)
library(adegraphics)
library(DT)

fluidPage(
    theme=shinytheme("flatly"),
        navbarPage("RLQ For Ecologists",
        #This is the full text of the summary tab panel 
          tabPanel("Summary",
                   br(),
                   p("Welcome to RLQ For Ecologists. This web application
                     is made for ecologists,
                     researchers and others that are interested in
                     multivariate analysis of trait-environment interaction.
                     This is a Shiny web application using R platform as
                     and the ade4 package (Dray et al., 2014,
                     Dray & Dufour, 2007) as statistical engine.
                     More information:"),
                   a("http://www.esapubs.org/archive/ecol/E095/002/suppl-1.pdf"),
                   br(),
                   br(),
                   p("THIS IS A BETA VERSION, THE DEVELOPER IS NOT RESPONSIBLE FOR
                          ANY RESULTS OF THE ANALYSIS AT THIS STAGE"),
                   hr(),
                   h3(strong("Analysis")),
                   h4("The input"),
                   p("RLQ analyses requires three matrices: Environmental variables (R),
                     species abundance (L),
                     and species traits (Q) as input.
                     Prior to the analyses the three tables required to be tested.
                     For R and Q tables, Principal Component Analysis (PCA) is used
                     to test the tables if the tables contain only numerical variables,
                     otherwise if the tables contain a mix of numerical and
                     categorical variables then Hill-Smith analysis will be applied.
                     The L table will be tested using the correspondence analysis (CA).
                     These tests will be automatically applied when the tables
                     are uploaded into the application"),
                   h4("Upload Data Option"),
                   p("The options inside the grey box, refer to the type of format
                     you are using in your file.
                     By default, the application will read a CSV file
                     with comma as separation.
                     However, if you use semicolon or
                     tab instead of commas then you can specify that
                     using the radio button.
                     Similarly, if your files contain quotes,
                     then you can specify the type of quotes you use
                     (single or double quotes)."),
                   h4("Upload Data"),
                   p("After you specified the upload data options then
                     you can start uploading your file.
                     This is done by clicking the 'choose file'
                     button under the title of each table.
                     After the files are uploaded you can inspect your data
                     by either clicking the 'show table' link or
                     by clicking the sub-tabs R table, L table, and Q table."),
                   h4("Result Parameters"),
                   p("This option allows you to select what type of result
                     output you want to generate.
                     You can either choose to generate plot only, generate summary,
                     or generate plot and summary by clicking on the checkboxes"),
                   h4("Run"),
                   p("Click on the 'Run!' button to start the analysis"),
                   hr(),
                   h3(strong("Results")),
                   p("When the option of create plot is selected, the outputs of
                     analysis will be drawn in the tab panel 'Plots'.
                     When the option of create summary is selected, the summary of
                     the output will be printed in the tab panel 'Summary'."),
                   h4("Plots"),
                   p("The plots of the output consist of biplot of Q-table,
                     biplot of the R-table, plot of R-axes, and plot of Q-axes."),
                   tags$div(tags$ul(
                     tags$li("biplot of Q-table: This plot shows the inertia of
                             the traits of species per plot. Here you can see the effect of
                             traits that define the species per plot."),
                     tags$li("biplot of the R-table: This plot shows the inertia of the
                             environmental factors to the species in plots."),
                     tags$li("R-axis: This plot shows the results of pca analysis of the
                             traits factors and how this factors are affecting
                             the species cluster"),
                     tags$li("Q-axis: this plot shows which environmental factors are affecting the
                             species clustering")
                     )),
                   h4("Summary"),
                   p("The summary tab panel shows the most important values of the
                     statistical output. This consists of: the inertia,
                     eigenvalue decomposition &
                     correlation"),
                  br(),
                  hr(),
                  p("created by: Yuga Wicaksono, contact: wicaksono.yuga@gmail.com")),
        # this is the UI layout of the analysis tab
          tabPanel("Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       h4("Upload data options"),
                       helpText("Please upload your files as comma separated value (.csv)"),
                       radioButtons("sep", "Separator",
                                    c(Comma=",",
                                      Semicolon=";",
                                      Tab="\t"),
                                    ','),
                       radioButtons("quote", "Quote",
                                    c(None="",
                                      'Double Quote'='"',
                                      'Single Quote'="'"),
                                    '"'),
                       checkboxInput("header", "Header", TRUE),
                       tags$hr(),
                       checkboxGroupInput("param",
                                          label= h4("Results Parameters"),
                                          choices= list("Create Plot"=1,
                                                        "Create Summary"= 2),
                                          selected=1),
                       tags$hr(),
                       actionButton("run","Run!",icon=NULL,width='40%',
                                    tags$style(HTML("#run{background-color:blue;
                                                    color:white;
                                                    font-weight:bold;
                                                    }")))
                        ),
              mainPanel(
                       tabsetPanel(id="upload",
                                   tabPanel("Configuration",
              fluidPage(
                        h3("Upload Data"),
                         br(),
              ## /////////////////// file input for R matrix /////////////////          
                        fluidRow(
                          column(6,
                                p(span("R table", style="font-size:16px;font-weight:bold")),
                                 helpText("A table with the environmental variables of the respective
                                          plots/ sites. The table has the structure of
                                          environmental variables (columns) x plots (rows)"),
                                 checkboxInput("Rcat","this table contains categorical variable",FALSE),
                                 fileInput("fileR","",
                                           accept=c("text/csv",
                                                    "text/comma-separated-values, text/plain",
                                                    ".csv"))),
                                  actionLink("showR","show table",icon("table")),
                          column(6,
                                 textOutput("textTR"),
                                 tags$head(tags$style("#textTR{color: green;
                                                      font-size: 13px;
                                                      font-style: italic;
                                                      }")
                                 ))),
                            hr(),
              ### ///////////////////// File input for L Matrix ///////////////////
              fluidRow(
                column(6,
                       p(span("L table", style="font-size:16px;font-weight:bold")),
                       helpText("A table with the abundance of species of
                                the respective plots/ sites.The table has the structure of
                                species abundance (columns) x plots (rows)"),
                       fileInput("fileL","",
                                 accept=c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv"))),
                        actionLink("showL","show table",icon("table")),
                column(6,
                       textOutput("textTL"),
                       tags$head(tags$style("#textTL{color: green;
                                            font-size: 13px;
                                            font-style: italic;
                                            }")
                       ))),
                      hr(),
              ###///////////////////  File input for Q matrix /////////////////
              fluidRow(
                column(6,
                      p(span("Q table", style="font-size:16px;font-weight:bold")),
                      helpText("A table with the species traits of
                                the respective species. The table has the structure of
                                species traits (columns) x species (rows)"),
                    checkboxInput("Qcat", "this table contains categorical variable", FALSE),
                    fileInput("fileQ","",
                              accept=c("text/csv",
                                  "text/comma-separated-values, text/plain",
                                    ".csv"))),
                          actionLink("showQ","show table",icon("table")),
                column(6,
                       textOutput("textTQ"),
                       tags$head(tags$style("#textTQ{color: green;
                                            font-size: 13px;
                                            font-style: italic;
                                            }")
                            )
                          )
                        )
                      )
                    ),
              tabPanel("R table", dataTableOutput("rTab")),
              tabPanel("L table", dataTableOutput("lTab")),
              tabPanel("Q table", dataTableOutput("qTab"))
              #tabPanel("lw_table",verbatimTextOutput("lw_table")), #<---- activate this for testing purposes
              #tabPanel("cw_table",verbatimTextOutput("cw_table"))  
                )
              )
            )
          ),
  ## the code for the results panel      
          tabPanel("Results",
              sidebarLayout(
                  sidebarPanel(
                      selectInput("selPlot",
                                  label = h4("Download result"),
                                  choices = list("biplot"=1,
                                                 "R Axes"= 2,
                                                 "Q Axes"= 3,
                                                selected =1)),
                         hr(),
                          checkboxGroupInput("imp.result", label=NULL,
                                              choices = list ("Save plot" =1,
                                                             "save summary"=2),
                                                              selected=1
                                                         ),
                          actionButton("ab1","Download Results",icon("download"),width="40%")
                          ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Plot",
                                     fluidPage(
                                       fluidRow(
                                         column(6,
                                                h3("biplot Q"),
                                                plotOutput("biplotQ")),
                                         column(6,
                                                h3("biplot R"),
                                                plotOutput("biplotR"))
                                       ),
                                       fluidRow(
                                         column(6,
                                                h3("R axes"),
                                                plotOutput("RAxes")),
                                         column(6,
                                                h3("Q axes"),
                                                plotOutput("QAxes"))
                                          )
                                       )),
                            tabPanel("Summary", verbatimTextOutput("summary"))
            )
          )
        )
      )
    )
  )