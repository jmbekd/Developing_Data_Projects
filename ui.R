library(shiny)
shinyUI(fluidPage(
  titlePanel("Determining Population Parameters for Mixtures of Normal Distributions"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("example", label = "Use example data?", value = TRUE),
      h5("Or"),
      fileInput("file1", "Choose CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      helpText("Note: All data will be converted to numeric and NA values will be omitted."),
      h5("Input file parameters"),
      checkboxInput("header", "Header", value = TRUE),
      radioButtons("sep", "Separator", c(Comma = ",", Semicolon = ";", Tab = "\t")),
      tags$hr(),
      selectInput("columns", "Please select desired column", ""),
      tags$hr(),
      h6(a("Code for this application", href = "https://github.com/jmbekd/Developing_Data_Projects"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Histogram",
                           h5("Mixtures of Normal Distributions"),
                           helpText("To determine whether the dataset may contain a mixture of normal
                                    distributions, examine a histogram or density plot (the blue line
                                    in the plot below) of the data to look for non-normality (e.g,
                                    multiple peaks, etc.). If the underlying data do not come from a
                                    normal distribution (e.g., concentration of pollutants in the
                                    environment), it may be possible to transform the data prior to
                                    analysis to make the transformed data \"approximately\" normal.
                                    Data transformations may also be useful if the range of values
                                    within the data set is >2-3 orders of magnitude."),
                           checkboxInput("log", label = h5("log10 Transform the Data?"),
                                         value = FALSE),
                           helpText("A common transformation for environmental pollutants (e.g.,
                                    arsenic in soil) is to log-transform the data."),
                           tags$hr(),
                           plotOutput("plot1", width = "400px", height = "400px"),
                           tags$hr(),
                           h5("Histogram Modifications"),
                           sliderInput("n", "Number of bins:", value = 20, min = 10, max = 50),
                           textInput("titlehist", "Title"),
                           textInput("xaxis", "X-Axis label"),
                           tags$hr(),
                           downloadButton("downloadPlot1", "Download Hist") ),
                   tabPanel("QQ Plot",
                            h5("Initial Estimate of Number of Populations and Relative Proportions"),
                            textInput("str", "Specify desired breakpoints separated by commas"),
                            helpText("To facilitate the determination of population parameters (i.e.,
                                     population means, population standard deviations, and the relative
                                     proportions of each population), please specify initial breakpoints.
                                     The initial breakpoints will be used to specify the number of
                                     potential populations to consider and as initial estimates of the
                                     relative proportions of each population. Breakpoints are the
                                     cumulative frequency values in the QQ plot below where the curve
                                     transitions from concave upwards to convex downwards or vise versa."),
                            tags$hr(),
                            plotOutput("plot2", width = "400px", height = "400px"),
                            tags$hr(),
                            h5("QQ Plot Modifications"),
                            textInput("titleqq", "Title"),
                            textInput("yaxis", "Y-Axis label"),
                            tags$hr(),
                            downloadButton("downloadPlot2", "Download QQ Plot")),
                   tabPanel("Mixture Model",
                            h5("Specified Breakpoints"),
                            h5(verbatimTextOutput("breakpoints")),
                            br(),
                            numericInput("seed", "To make the analysis reproducible,
                                         specify a seed value.",
                                         value = 1),
                            tags$hr(),
                            h5("Calculate Population Parameters"),
                            actionButton("calcButton", "Press to Calculate or Recalculate"),
                            br(),
                            br(),
                            tableOutput("model")
                            ),
                  tabPanel("Simulated Data",
                           sliderInput("reps", "Select desired number of simulations",
                                       min = 1, max = 25, value = 10),
                           helpText("Depending on the amount of data, this plot below may take some
                                    time to load and/or update."),
                           tags$hr(),
                           plotOutput("plot3", width = "400px", height = "400px"),
                           tags$hr(),
                           h5("QQ Plot Modifications"),
                           textInput("titleqqSim", "Title"),
                           textInput("yaxisSim", "Y-Axis label"),
                           tags$hr(),
                           downloadButton("downloadPlot3", "Download QQ Plot of Simulated Data"))
                  )
      )
    )
  )
)