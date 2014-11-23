if(!require("mixtools")) install.packages("mixtools", repos = "http://cran.r-project.org")
library(mixtools)
if(!require("data.table")) install.packages("data.table", repos = "http://cran.r-project.org")
library(data.table)

shinyServer(
  function(input, output, session) {

#### Code for loading example data or user specified data ----
    dataset <- reactive({
      inFile <- input$file1
      if(is.null(input$file1)) {
        if (input$example) {
          return(exampleData)
        }
        return() }
      read.csv(inFile$datapath, header = (input$header == 1), sep = input$sep, quote = input$quote,
                       stringsAsFactors = FALSE)
    })

    outVar <- reactive({
      names(dataset())
    })

    observe({
      updateSelectInput(session, "columns", choices = outVar())
    })

    datasetInput <- reactive({
      df <- dataset()[[input$columns]]
      if(!is.null(df))
        df <- na.omit(as.numeric(as.character(df)))
      })

#### Code for plotting histograms ----
    plothist <- function() {
      df <- datasetInput()
      if(!is.null(df)) {
        if (input$log)
          df <- log10(df)
        xaxis <- ifelse(input$xaxis == "", input$columns, input$xaxis)
        myname <- ifelse(input$titlehist == "",
                         paste0("Histogram of ", input$columns),
                         input$titlehist)
        hist(df, breaks = input$n, main = myname,
             xlab = ifelse(input$log, paste0("log10 ", xaxis), xaxis), freq = FALSE)
        lines(density(df), col = "blue")
      }
    }

    output$plot1 <- renderPlot({ plothist() })

    output$downloadPlot1 <- downloadHandler(
      filename = function() {paste0("hist", ".png")},
      content = function(file) {
        png(file, height = 480, width = 480, bg = "white")
        print(plothist())
        dev.off()
      })

#### Code for preparing QQ Plot and setting breakpoints ----
qqPlot <- function() {
  df <- datasetInput()
  if(!is.null(df)) {
    if (input$log)
      df <- log10(df)
    yaxis <- ifelse(input$yaxis == "", input$columns, input$yaxis)
    myname <- ifelse(input$titleqq == "",
                     paste0("Normal Q-Q Plot of ", input$columns),
                     input$titleqq)
    breaks <- as.numeric(unlist(strsplit(input$breaks, ","))) / 100
    percents <- c(0.1, 1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98, 99, 99.9)
    qq_data <- as.data.frame(qqnorm(df, plot.it = FALSE))
    plot(0,
               type = "n",
               xlim = range(qnorm(percents / 100)),
               ylim = range(df),
               xaxt = "n",
               yaxt = "n",
               xlab = "Cumulative Frequency (%)",
               ylab = ifelse(input$log, paste0("log10 ", yaxis), yaxis),
               main = myname)
          points(qq_data, pch = 19)
          axis(1, at = qnorm(percents / 100), labels = as.character(percents), cex.axis = 0.75)
          ylabels <- seq(floor(range(df)[1]), ceiling(range(df)[2]), by = 1)
          ifelse(input$log,
                 axis(2, at = ylabels, labels = as.character(round(10^ylabels), 4),
                      las = 2, cex.axis = 0.75),
                 axis(2, las = 2, cex.axis = 0.75))
          abline(v = qnorm(breaks), col = "red", lwd = 2)
  }
}

output$plot2 <- renderPlot({ print(qqPlot()) })

output$downloadPlot2 <- downloadHandler(
  filename = function() {paste0("qqplot", ".png")},
  content = function(file) {
    png(file, height = 480, width = 480, bg = "white")
      print(qqPlot())
    dev.off()
  })

output$breakpoints <- renderText(input$breaks)

#### Code for Calculating the Population Parameters for Mixtures of Normal Distributions ----
mix_model <- function() {
  df <- datasetInput()
  if(!is.null(df)) {
    if (input$log)
      df <- log10(df)
    ## Calculate initial estimates for lambda, mu, and sigma using user specified breakpoints
    i_breaks <- as.numeric(unlist(strsplit(input$breaks, ","))) / 100
    breaks <- qnorm(i_breaks)
    qq_data <- as.data.frame(qqnorm(df, plot.it = FALSE))
    qq_data$breaks <- cut(qq_data$x,
                          breaks = c(min(qq_data$x), breaks, max(qq_data$x)),
                          right = FALSE,
                          include.lowest = TRUE,
                          ordered_result = TRUE)
    populations <- split(qq_data$y, qq_data$breaks)
    pop_elements <- table(qq_data$breaks)
    lambda <- pop_elements / sum(pop_elements)

  #### Run normalmixEM from mixtools package ----
    set.seed(input$seed)
    normalmixEM(df,
                lambda = lambda,
                mu = sapply(populations, mean),
                sigma = sapply(populations, sd),
                maxit = 10000,
                maxrestarts = 20,
                epsilon = 0.001,
                arbmean = TRUE,
                arbvar = TRUE)
  }
}

values <- reactiveValues(table = NA)

observe({
  if(input$calcButtonTable > 0) {
    values$table <- isolate({
      mm <- mix_model()
      model <- as.data.frame(rbind(mm$lambda, mm$mu, mm$sigma))
      row.names(model) <- c("Relative Proportion", "Population Mean", "Population Standard Deviation")
      colnames(model) <- paste0("Population ",1:length(mm$lambda))
      model
    })
  }
})

output$model <- renderTable({
  if (input$breaks == "") stop("Please specify desired inflection points in QQ Plot tab.")
  if (input$calcButtonTable == 0)
    return()
  values$table
  })

output$downloadModel <- downloadHandler(
  filename = function() {paste0("pop_params", ".csv")},
  content = function(file) {
    write.csv(values$table, file)
  })

#### Code for Simulating the Data and comparing it to the Original Dataset ----
sim_data <- function() {
  df <- datasetInput()
  if(!is.null(df)) {
    if (input$log)
      df <- log10(df)
    num <- length(df)
    model <- t(values$table)
    mylambda <- model[, 1]
    mu <- model[, 2]
    sigma <- model[, 3]
    n_pop <- length(mylambda)
    z <- sort(sample(n_pop, num, replace = TRUE, prob = mylambda))
    m <- matrix(rnorm(num * input$reps, mean = rep(mu[z], input$reps), sd = rep(sigma[z], input$reps)),
                num, input$reps)
    new_data <- data.table("model_data" = rep(paste0("V", 1:input$reps), each = num),
                           "values" = as.vector(m),
                           "src_pop"  = rep(z, input$reps))
    setkey(new_data, model_data, values)
  }
}

qqPlotSim <- function() {
  df <- datasetInput()
  if(!is.null(df)) {
    if (input$log)
      df <- log10(df)
    yaxis <- ifelse(input$yaxisSim == "", input$columns, input$yaxisSim)
    myname <- ifelse(input$titleqqSim == "",
                     paste0("Normal Q-Q Plot of ", input$columns, " Data\nand ",
                            input$reps, " Simulated Data Realizations"),
                     input$titleqqSim)
    percents <- c(0.1, 1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98, 99, 99.9)
    qq_data <- as.data.frame(qqnorm(df, plot.it = FALSE))
    simulated_data <- sim_data()
    plot(0,
         type = "n",
         xlim = range(qnorm(percents / 100)),
         ylim = range(df),
         xaxt = "n",
         yaxt = "n",
         xlab = "Cumulative Frequency (%)",
         ylab = ifelse(input$log, paste0("log10 ", yaxis), yaxis),
         main = myname)
    axis(1, at = qnorm(percents / 100), labels = as.character(percents), cex.axis = 0.75)
    ylabels <- seq(floor(range(df)[1]), ceiling(range(df)[2]), by = 1)
    ifelse(input$log,
           axis(2, at = ylabels, labels = as.character(round(10^ylabels), 4),
                las = 2, cex.axis = 0.75),
           axis(2, las = 2, cex.axis = 0.75))
    sapply(simulated_data$model_data,
           function(x) {lines(simulated_data[J(x), qqnorm(values, plot.it = FALSE)], col = "grey")})
    points(qq_data)
    capture.output({
      legend("topleft",
             legend = c(paste0("Original Data"), paste0("Simulated Data")),
             pch = c(1, NA),
             lty = c(NA, 1),
             col = c(1, "grey"),
             inset = 0.01,
             bty = "n")
      })
  }
}

output$plot3 <- renderPlot({
  if (input$createPlot == 0)
    return()
  simQQPlot <- isolate(qqPlotSim())
  print(simQQPlot)
})

output$downloadPlot3 <- downloadHandler(
  filename = function() {paste0("qqplot_sim", ".png")},
  content = function(file) {
    png(file, height = 480, width = 480, bg = "white")
    print(qqPlotSim())
    dev.off()
  })

} )

## Example data
exampleData <- structure(list(
  faithful.eruptions = c(3.6, 1.8, 3.333, 2.283,
                         4.533, 2.883, 4.7, 3.6, 1.95, 4.35, 1.833, 3.917, 4.2, 1.75,
                         4.7, 2.167, 1.75, 4.8, 1.6, 4.25, 1.8, 1.75, 3.45, 3.067, 4.533,
                         3.6, 1.967, 4.083, 3.85, 4.433, 4.3, 4.467, 3.367, 4.033, 3.833,
                         2.017, 1.867, 4.833, 1.833, 4.783, 4.35, 1.883, 4.567, 1.75,
                         4.533, 3.317, 3.833, 2.1, 4.633, 2, 4.8, 4.716, 1.833, 4.833,
                         1.733, 4.883, 3.717, 1.667, 4.567, 4.317, 2.233, 4.5, 1.75, 4.8,
                         1.817, 4.4, 4.167, 4.7, 2.067, 4.7, 4.033, 1.967, 4.5, 4, 1.983,
                         5.067, 2.017, 4.567, 3.883, 3.6, 4.133, 4.333, 4.1, 2.633, 4.067,
                         4.933, 3.95, 4.517, 2.167, 4, 2.2, 4.333, 1.867, 4.817, 1.833,
                         4.3, 4.667, 3.75, 1.867, 4.9, 2.483, 4.367, 2.1, 4.5, 4.05, 1.867,
                         4.7, 1.783, 4.85, 3.683, 4.733, 2.3, 4.9, 4.417, 1.7, 4.633,
                         2.317, 4.6, 1.817, 4.417, 2.617, 4.067, 4.25, 1.967, 4.6, 3.767,
                         1.917, 4.5, 2.267, 4.65, 1.867, 4.167, 2.8, 4.333, 1.833, 4.383,
                         1.883, 4.933, 2.033, 3.733, 4.233, 2.233, 4.533, 4.817, 4.333,
                         1.983, 4.633, 2.017, 5.1, 1.8, 5.033, 4, 2.4, 4.6, 3.567, 4,
                         4.5, 4.083, 1.8, 3.967, 2.2, 4.15, 2, 3.833, 3.5, 4.583, 2.367,
                         5, 1.933, 4.617, 1.917, 2.083, 4.583, 3.333, 4.167, 4.333, 4.5,
                         2.417, 4, 4.167, 1.883, 4.583, 4.25, 3.767, 2.033, 4.433, 4.083,
                         1.833, 4.417, 2.183, 4.8, 1.833, 4.8, 4.1, 3.966, 4.233, 3.5,
                         4.366, 2.25, 4.667, 2.1, 4.35, 4.133, 1.867, 4.6, 1.783, 4.367,
                         3.85, 1.933, 4.5, 2.383, 4.7, 1.867, 3.833, 3.417, 4.233, 2.4,
                         4.8, 2, 4.15, 1.867, 4.267, 1.75, 4.483, 4, 4.117, 4.083, 4.267,
                         3.917, 4.55, 4.083, 2.417, 4.183, 2.217, 4.45, 1.883, 1.85, 4.283,
                         3.95, 2.333, 4.15, 2.35, 4.933, 2.9, 4.583, 3.833, 2.083, 4.367,
                         2.133, 4.35, 2.2, 4.45, 3.567, 4.5, 4.15, 3.817, 3.917, 4.45,
                         2, 4.283, 4.767, 4.533, 1.85, 4.25, 1.983, 2.25, 4.75, 4.117,
                         2.15, 4.417, 1.817, 4.467, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  faithful.waiting = c(79L, 54L, 74L, 62L, 85L, 55L, 88L, 85L,
                       51L, 85L, 54L, 84L, 78L, 47L, 83L, 52L, 62L, 84L, 52L, 79L, 51L,
                       47L, 78L, 69L, 74L, 83L, 55L, 76L, 78L, 79L, 73L, 77L, 66L, 80L,
                       74L, 52L, 48L, 80L, 59L, 90L, 80L, 58L, 84L, 58L, 73L, 83L, 64L,
                       53L, 82L, 59L, 75L, 90L, 54L, 80L, 54L, 83L, 71L, 64L, 77L, 81L,
                       59L, 84L, 48L, 82L, 60L, 92L, 78L, 78L, 65L, 73L, 82L, 56L, 79L,
                       71L, 62L, 76L, 60L, 78L, 76L, 83L, 75L, 82L, 70L, 65L, 73L, 88L,
                       76L, 80L, 48L, 86L, 60L, 90L, 50L, 78L, 63L, 72L, 84L, 75L, 51L,
                       82L, 62L, 88L, 49L, 83L, 81L, 47L, 84L, 52L, 86L, 81L, 75L, 59L,
                       89L, 79L, 59L, 81L, 50L, 85L, 59L, 87L, 53L, 69L, 77L, 56L, 88L,
                       81L, 45L, 82L, 55L, 90L, 45L, 83L, 56L, 89L, 46L, 82L, 51L, 86L,
                       53L, 79L, 81L, 60L, 82L, 77L, 76L, 59L, 80L, 49L, 96L, 53L, 77L,
                       77L, 65L, 81L, 71L, 70L, 81L, 93L, 53L, 89L, 45L, 86L, 58L, 78L,
                       66L, 76L, 63L, 88L, 52L, 93L, 49L, 57L, 77L, 68L, 81L, 81L, 73L,
                       50L, 85L, 74L, 55L, 77L, 83L, 83L, 51L, 78L, 84L, 46L, 83L, 55L,
                       81L, 57L, 76L, 84L, 77L, 81L, 87L, 77L, 51L, 78L, 60L, 82L, 91L,
                       53L, 78L, 46L, 77L, 84L, 49L, 83L, 71L, 80L, 49L, 75L, 64L, 76L,
                       53L, 94L, 55L, 76L, 50L, 82L, 54L, 75L, 78L, 79L, 78L, 78L, 70L,
                       79L, 70L, 54L, 86L, 50L, 90L, 54L, 54L, 77L, 79L, 64L, 75L, 47L,
                       86L, 63L, 85L, 82L, 57L, 82L, 67L, 74L, 54L, 83L, 73L, 73L, 88L,
                       80L, 71L, 83L, 56L, 79L, 78L, 84L, 58L, 83L, 43L, 60L, 75L, 81L,
                       46L, 90L, 46L, 74L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  zinc = c(72, 42, 59, 61, 315, 95.6, 151, 43.3, 42.5, 56.4, 22.8, 71.6, 40.3,
           67.1, 30.4, 194, 17.4, 138, 140, 2120, 227, 290, 89, 560, 370,
           701, 47, 62, 55, 13, 23, 2400, 1600, 850, 170, 230, 37.6, 42,
           49.9, 46, 24.4, 32.5, 27.5, 46.5, 43, 42.1, 83.1, 83, 33.7, 47.4,
           65.8, 67.7, 50.5, 58.2, 46.2, 30.5, 37.8, 35.6, 47.8, 40.6, 40,
           68.7, 53.4, 37.7, 42.6, 38.6, 37.3, 94.9, 31.7, 34.7, 39.9, 82.1,
           169, 96.9, 829, 78, 32, 20.8, 85, 20.9, 18, 16.2, 14.9, 97.5,
           62.7, 62.2, 80.3, 71.4, 17.3, 204, 33.6, 21.1, 30.2, 28, 29.8,
           51.9, 65.1, 150, 450, 80, 110, 81, 30, 34, 69.1, 66.9, 68.4,
           74.1, 55.2, 61.7, 55.7, 60.1, 57, 24.1, 32, 36.4, 43.3, 49.1,
           42.5, 78.5, 81.6, 33.8, 42, 34, 42.6, 49.8, 39.8, 55.6, 73, 69.2,
           54, 30.4, 18.4, 29.3, 49.6, 42, 93.9, 69.8, 52.7, 18.1, 21.7,
           15.4, 60.4, 57.4, 76.3, 19.4, 20.3, 17.3, 18.8, 15.2, 38.5, 43.1,
           23.8, 27.7, 32.7, 28.1, 43.4, 75.4, 102.1, 53.2, 56.4, 45.4,
           56.3, 63.1, 54.9, 53.5, 45.3, 102.3, 58.9, 54.8, 56.6, 66.2,
           49, 59.6, 77.7, 77, 89.8, 92.4, 77.9, 99.3, 21.8, 23.7, 21.2,
           25.1, 17, 19.8, 14.2, 22.7, 29.9, 18.1, 32.8, 23.8, 26.3, 18,
           59, 81, 23.5, 20.8, 21.8, 24.1, 30.4, 30.6, 112, 128, 76, 408,
           1010, 224, 3370, 3086, 796, 1700, 950, 1000, 284, 123, 360, 730,
           910, 176, 220, 3220, 172, 580, 584, 5400, 2100, 100, 382, 839,
           937, 368, 2230, 80, 190, 78, 140, 77, 100, 31, 69, 92, 88, 82,
           46.3, 50.1, 40, 39.4, 44.3, 51, 66.2, 73.2, 68.7, 69.1, 59, 37.6,
           38.2, 75.4, 61.9, 40.8, 62.4, 46.7, 48.9, 40.1, 81.6, 78, 55.4,
           51.3, 48.1, 68.3, 60, 63.3, 56.6, 88.5, 58.9, 51.9, 64.8, 67.1,
           62.2, 108.2, 56.6, 57.4, 53.7, 63.4, 56, 60.3, 128, 27.5, 56.5,
           68.2, 42.3, 68.9, 92.8, 42.4, 68.6, 48.2, 53.7, 88.5, 29.9, 40.5,
           38, 80.9, 40.1, 43.4, 32.2, 40.5, 41.6, 68.2, 38.9, 49.6, 49.6,
           26.6, 30.2, 42, 36.7, 73.1, 70.7, 51.1, 32.5, 54.7, 50.6, 45.4,
           29, 45.1, 44.8, 32.2, 33.8, 59.4, 75.5, 66, 8.2, 9.3, 33.9, 52.2,
           74.9, 52.9, 9.9, 40.5, 74.1, 75.2, 85, 23.4, 55.4, 59.3, 64.2,
           76.8, 77.9, 6.8, 30.8, 54.5, 51.9, 52.6, 12.3, 35.8, 33.3, 64.5,
           73.3, 64.7, 40.8, 71.7, 55.7, 49.3, 81.3, 95.9, 10.2, 85.7, 56.7,
           42, 39, 33.6, 64.6, 73.1, 70.8, 33.5, 44.2, 48.6, 63.7, 72.7,
           55.3, 20.1, 40.4, 44.7, 48.1, 38.7, 76.2, 43.1, 68.2, 94.3, 65.1,
           65.2, 43.9, 62, 54.2, 10.1, 41.5, 38.1, 30.7, 32.9, 27.1, 62.8,
           44.5, 62.7, 38.9, 53.4, 61.4, 65.3, 18.3, 71.7, 41.5, 33.9, 47.6,
           24.9, 26.4, 74.7, 39.1, 50.3, 53.5, 58.3, 76.6, 26.9, 54.3, 60.5,
           38.4, 29.7, 34, 25.4, 25.4, 56.8, 55, 52.8, 64.6, 14.8, 43.4,
           41.5, 38.9, 40.5, 57, 14.7, 57.7, 52.2, 35.6, 29.9, 71.1, 35.6,
           114.6, 125, 63.9, 97.7, 61.5, 35.7, 63.7, 47, 49.8, 43.3, 63,
           45.7, 61.7, 47.9, 31.2, 53.4, 44.2, 54.9, 50.1, 58.1, 57.3, 55.6,
           54.1, 48.1, 67.7, 55.6, 62.3, 34.9, 73.1, 121, 15.2, 41.7, 42.5,
           40.4, 40.6, 40.3, 71.9, 32.4, 46.5, 63.2, 65.7, 74.9, 47.8, 54.8,
           51.3, 62.6, 42.9, 61.6, 57.2, 25.8, 74.5, 68.8, 64, 60.7, 53.3,
           48.6, 63.2, 74.6, 46.8, 66.3, 72.4, 62, 36.1, 64, 30.1, 91.6,
           36.7, 36, 38, 40.2, 60.9, 50, 74, 33, 60.9, 47, 43.5, 160, 55.6,
           66, 72, 52, 37, 52, 43, 67, 69.1, 53, 61.2, 61, 50.6, 67, 34,
           40, 51, 45, 39, 52.3, 36, 50.7, 58, 48.4, 57, 110, 55, 48, 55,
           53, 59, 57, 27, 30, 51, 55, 41, 51, 54, 53, 49, 61, 52, 52, 36,
           51, 53, 47, 41, 100, 45, 37, 42, 42, 41, 44, 39, 49, 51, 40,
           33, 45, 39, 45, 58, 40, 36, 39, 48, 48, 43, 41, 30, 31, 45, 48,
           51, 35, 48, 52, 46, 46, 46, 47, 43, 47, 45, 44, 59, 53, 53, 66,
           62, 70, 46, 52, 47, 62, 59.9, 60, 54, 53, 54, 71, 60, 54, 35,
           65, 53, 30, 32, 39, 47, 37, 32, 45, 54, 27, 26, 48, 61, 66, 45,
           47.4, 24, 42.3, 40.2, 30.7, 46.6, 42.2, 27.6, 33.4, 52.5, 53.2,
           48.4, 60.5, 59.3, 57.1, 57.7, 43.7, 37.9, 67.7, 66.8, 64.8, 81.3,
           64.1, 68, 83.8, 65.9, 60.6, 68.4, 87.1, 27, 41, 36, 36, 62, 42,
           58, 33.2, 30, 35, 23, 44, 31.7, 44.6, 47.3, 52, 52.4, 55.9, 60,
           60.2, 42.7, 34.7, 50.5, 30.4, 55.9, 29.8, 51, 40.3, 33.1, 38.4,
           42.2, 52.9, 53.4, 41.6, 84.1, 14.9, 39, 37, 27.8, 32.3, 41.8,
           40.4, 45.4, 42.4, 28.2, 52.3, 59.5, 57.2, 28.8, 23.9, 44.7, 62.8,
           48.5, 64.4, 26.2, 53.8, 49.1, 56.4, 50.1, 19.9, 32.5, 62.1, 73.3,
           72.6, 39.8, 29.1, 59.1, 31.5, 81.5, 125, 44.6, 40.8, 53.3, 53.6,
           42.1, 38.8, 38.1, 35.9, 58, 58.5, 273, 183, 34.6, 13.8, 38.2,
           44.2, 41, 49, 44.6, 93, 160, 160, 130, 380, 120, 90, 62, 58,
           74, 60, 79, 1700, 260, 910, 1200, 2295, 36, 95, 38, 94, 25, 190,
           61.95, 82, 54, 20, 34.7, 22, 16.8, 17.9, 206, 49.1, 24.5, 33,
           25, 19, 13, 112, 32.4, 22.3, 18, 29, 16, 19, 15, 13, 10, 16,
           44, 27, 18, 16, 25, 29, 27, 22, 20, 22, 16, 21, 21, 14, 15, 17,
           15, 110, 30, 11, 42, 42, 23, 14, 15, 28, 17, 23, 29, 22, 237,
           111.1, 143.6, 102.2, 137.7, 323, 459, 123.4, 652, 148, 412, 68.9,
           26.4, 108, 20.4, 84.7, 22.8, 74.8, 20.7, 281, 26.3, 78.4, 39.3,
           58.4, 36.9, 20.8, 21.1, 25.3, 19.6, 36.1, 130, 98, 160, 270,
           81.4, 25.9, 25.5, 23.6, 31.5, 50.3, 37.5, 39.3, 40.5, 33.7, 185,
           192, 51.1, 256, 32.5, 64.9, 102, 69.9, 164, 77.7, 124, 33.3,
           91.4, 46.2, 150, 168, 110, 68, 50, 19, 120, 27, 360, 30, 33,
           36, 19, 20, 29, 26, 300, 480, 410, 730, 790, 830, 890, 56, 11,
           29, 48, 52, 40, 38, 75, 74.2, 25.2, 130, 21.9, 31.3, 123, 44,
           200, 250, 430, 48, 42, 43, 39, 63, 10.6, 13, 50, 47, 12, 41,
           27, 34, 46, 43, 39, 47, 44, 82, 68, 49, 34, 70, 58, 36, 23, 45,
           44, 42, 31, 31, 85, 98, 790, 490, 1900, 8200, 2600, 1300, 3100,
           460, 2700, 250, 29, 13, 25, 30, 18, 23, 20, 50, 17, 20, 19, 41,
           18, 20, 6, 5, 4, 4, 15, 4, 4, 8, 8, 8, 5, 4, 8, 7, 24, 67, 21,
           21, 33, 61, 40, 34.9, 162, 28.6, 24.5, 53.9, 51.6, 39.5, 123,
           25.7, 61, 27.7, 55.8, 55.4, 38.7, 18.5, 54.9, 62.5, 29.1, 24,
           20.3, 19.6, 22.6, 168, 258, 183, 691, 865, 42.6, 38.2, 35.6,
           68.4, 48, 126, 62.2, 23.2, 22.7, 29.2, 34, 23.8, 24.7, 221, 43.4,
           20.8, 29.7, 62.2, 42.5, 52.6, 28.5, 28, 60.1, 33.7, 23.3, 62.2,
           61.3, 42.7, 29, 180, 152, 331, 54.3, 33.9, 31.3, 37.2, 42.8,
           16.4, 37, 100, 60.1, 31.2, 28.8, 73.2, 72, 75.9, 25, 63.22, 127.66,
           104.45, 35.8, 540, 51, 50, 140, 62, 79, 88.7, 48.2, 55.4, 526,
           191, 157, 18.4, 21.6, 132, 71.3, 136, 24.4, 54.1, 25.3, 42.2,
           20.2, 22.4, 48.1, 172, 30.6, 15.9, 15.7, 419, 86.2, 720, 613,
           790, 290, 37, 42, 36, 28, 36, 40, 42, 41, 38, 37, 34, 51, 70.3,
           46.4, 30, 34, 61, 43, 38, 28, 30, 26, 26, 39, 39, 34, 30, 54,
           36, 31, 28, 33, 32, 82, 50, 32, 34, 33, 130, 62, 47, 29, 43,
           26, 46, 45, 79, 37, 41, 35, 44, 48, 61, 30, 58.9, 14, 36, 790,
           59, 43, 110, 65, 27, 25, 29, 1170, 834, 33.1, 27.6, 30.4, 32.8,
           21.5, 26.8, 49, 29.4, 34, 48.6, 30.8, 28.3, 38.5, 37.5, 32.2,
           39, 38.6, 30.5, 35.6, 30.1, 28.7, 33.2, 29.4, 28.4, 52.6, 34.1,
           20.2, 25.5, 36.2, 38.6, 25.7, 38.1, 41.1, 36.2, 39.2, 58, 52.1,
           202, 73.7, 39.2, 28.9, 26.2, 30.4, 37, 32.4, 33.7, 38.4, 36.3,
           34.6, 31.6, 49.3, 37.8, 46.9, 40.7, 34.7, 32, 33.7, 42.5, 38.1)),
  .Names = c("faithful.eruptions", "faithful.waiting", "zinc"), class = "data.frame", row.names = c(NA, -1257L))