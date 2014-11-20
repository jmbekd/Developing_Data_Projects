if(!require("mixtools")) install.packages("mixtools", repos = "http://cran.r-project.org")
library(mixtools)
if(!require("data.table")) install.packages("data.table", repos = "http://cran.r-project.org")
library(data.table)

shinyServer(
  function(input, output, session) {

    dataset <- reactive({
      df <- get("faithful")
      if (input$example & is.null(input$file1)) {
        df <- get("faithful")
        return(df) }
      inFile <- input$file1
      df <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote,
                     stringsAsFactors = FALSE)
      df <- as.numeric(as.character(df))
      if (any(is.na(df))) {
        df <- na.omit(df)
        cat("Removed NA values\n")
      }
    })

    outVar <- reactive({
      mydata <- dataset()
      name <- names(mydata)
    })

    observe({
      updateSelectInput(session, "columns", choices = outVar())
    })

    datasetInput <- reactive(dataset()[[input$columns]])

    plothist <- function() {
      df <- datasetInput()
      if(!is.null(df)) {
        if (input$log) df <- log10(df)
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

qqPlot <- function() {
  yaxis <- ifelse(input$yaxis == "", input$columns, input$yaxis)
  myname <- ifelse(input$titleqq == "",
                   paste0("Normal Q-Q Plot of ", input$columns),
                   input$titleqq)
  df <- datasetInput()
  if(!is.null(df)) {
    if (input$log) df <- log10(df)
    b <- as.numeric(unlist(strsplit(input$str, ","))) / 100
    percents <- c(0.1, 1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98, 99, 99.9)
    qq_data <- as.data.frame(qqnorm(df, plot.it = FALSE))
    p <- {plot(0,
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
          abline(v = qnorm(b), col = "red", lwd = 2)}
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

output$breakpoints <- renderText(input$str)

i_mix_model <- function() {
  ## Calculates the initial mixing model solution --
  # data = vector of the data
  # breaks = vector of the breakpoints (values between 0 and 100)
  # name = name of the analyte for which the mixing model is calculated
  # epsilon = an input variable to normalmixEM

  #### Calculate initial estimates for lambda, mu, and sigma ----
  i_breaks <- as.numeric(unlist(strsplit(input$str, ","))) / 100
  breaks <- qnorm(i_breaks)
  df <- as.numeric(as.character(datasetInput()))
  if(!is.null(df)) {
    if (input$log) df <- log10(df)
    qq_data <- as.data.frame(qqnorm(df, plot.it = FALSE))
    qq_data$breaks <- cut(qq_data$x,
                          breaks = c(min(qq_data$x), breaks, max(qq_data$x)),
                          right = FALSE,
                          include.lowest = TRUE,
                          ordered_result = TRUE)
    populations <- split(qq_data$y, qq_data$breaks)
    pop_elements <- table(qq_data$breaks)
    lambda <- pop_elements / sum(pop_elements)

    set.seed(input$seed)

    #### Run normalmixEM ----
    mm <- normalmixEM(df,
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

values <- reactiveValues(variable = NA)

observe({
  if(input$calcButton > 0) {
    values$variable <- isolate({
      mm <- i_mix_model()
      model <- as.data.frame(rbind(mm$lambda, mm$mu, mm$sigma))
      row.names(model) <- c("Relative Proportion", "Population Mean", "Population Standard Deviation")
      colnames(model) <- paste0("Population ",1:length(mm$lambda))
      model
    })
  }
})

output$model <- renderTable({
  if (input$str == "") stop("Please specify desired breakpoints in QQ Plot tab.")
  if (input$calcButton == 0) return()
  values$variable})

sim_data <- function() {
  df2 <- datasetInput()
  num <- length(df2)
  model <- t(values$variable)
  mylambda <- model[, 1]
  mu <- model[, 2]
  sigma <- model[, 3]
  n_pop <- length(mylambda)
  z <<- sort(sample(n_pop, num, replace = TRUE, prob = mylambda))
  m <- matrix(rnorm(num * input$reps, mean = rep(mu[z], input$reps), sd = rep(sigma[z], input$reps)),
              num, input$reps)
  new_data <- data.table("model_data" = rep(paste0("V", 1:input$reps), each = num),
                         "values" = as.vector(m),
                         "src_pop"  = rep(z, r))
  setkey(new_data, model_data, values)
}

qqPlotSim <- function() {
  yaxis <- ifelse(input$yaxisSim == "", input$columns, input$yaxisSim)
  myname <- ifelse(input$titleqqSim == "",
                   paste0("Normal Q-Q Plot of ", input$columns, " Data\nand ",
                          input$reps, " Simulated Data Realizations"),
                   input$titleqqSim)
  df <- datasetInput()
  if(!is.null(df)) {
    if (input$log) df <- log10(df)
    percents <- c(0.1, 1, 2, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 98, 99, 99.9)
    qq_data <- as.data.frame(qqnorm(df, plot.it = FALSE))
    num <- nrow(qq_data)
    model <<- t(values$variable)
    simulated_data <<- sim_data()
    p <- {plot(0,
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
          sapply(simulated_data$model_data,
                 function(x) {lines(simulated_data[J(x), qqnorm(values, plot.it = FALSE)], col = "grey")})
          points(qq_data)
          capture.output(legend("topleft",
                                legend = c(paste0("Original Data"), paste0("Simulated Data")),
                                pch = c(1, NA),
                                lty = c(NA, 1),
                                col = c(1, "grey"),
                                inset = 0.01,
                                bty = "n"))
    }
  }
}

output$plot3 <- renderPlot({ print(qqPlotSim()) })

output$downloadPlot3 <- downloadHandler(
  filename = function() {paste0("qqplot_sim", ".png")},
  content = function(file) {
    png(file, height = 480, width = 480, bg = "white")
    print(qqPlotSim())
    dev.off()
  })

} )
