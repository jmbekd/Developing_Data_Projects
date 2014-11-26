---
title       : Determining Population Parameters for Mixtures of Normal Distributions
subtitle    : 
author      : John Montgomery-Brown
job         : Coursera
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : github      # 
widgets     : [mathjax]            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Problem: Estimating Population Parameters for Subpopulations within a Geochemical Data Set

With most sets of environmental and/or geochemical samples, it is fairly common to have multiple geochemical populations represented. For example, 
- soil at a site may contain one or more background populations of a particular analyte and also be contaminated by an anthropogenic population of the same analyte.
- in an ore deposit, several ore types may be mixed, i.e., one may contain veinlets of pure mineral (e.g., copper) and another may have lower concentrations of the analyte of interest more evenly dispersed in the rock.

To obtain accurate estimates of the parameters of interest (i.e., the degree of contamination due to anthropogenic activities, the average grade of a precious metal deposit), it is essential to be able to identify (e.g., background versus contaminated) and characterize (e.g., estimate the population mean and standard deviation) the subpopulations within a given data set.

--- .class #id 

## Problem: Estimating Population Parameters for Subpopulations within a Geochemical Data Set

While the importance of accurately characterizing the subpopulations within a data set is clearly recognized within the [mining industry](http://books.google.com/books?id=oo7rCrFQJksC&pg=PA173&lpg=PA173&dq=mixture+normal+distributions+sinclair&source=bl&ots=-uBTeXjIR4&sig=KZxWDsCq7BJbroM5FFoVk3kHFjs&hl=en&sa=X&ei=txlxVLWnJIj9iAKjq4G4BA&ved=0CC4Q6AEwAw#v=onepage&q=mixture%20normal%20distributions%20sinclair&f=false) (where the characterization of disctinct ore and waste types is key to assessing economic viability), this characterization is less frequently performed in the field of environmental remediation. 

As a result, few tools have been developed to assist environmental professionals with this type of analysis. 

--- .class #id 

## Shiny Application

For this project, I have developed a [Shiny Application](https://jmbekd.shinyapps.io/Developing_Data_Projects/) that guides users through the process of:
- investigating and transforming (if necessary) the original data, and 
- selecting inflection points on a QQ plot to separate the mixture of subpopulations into the component subpopulations.

Using this information the [Shiny application](https://jmbekd.shinyapps.io/Developing_Data_Projects/) 
- uses the `normalmixEM` [expectation-maximization](http://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm) function within the `mixtools` package to estimate maximum likelihood population parameters for the subpopulations, and 
- simulates example data sets from the estimated population parameters for comparison against the original data distribution. 

Additional details on transforming the data and selecting inflection points are presented on the following slide.

The code used to create the Shiny application is [here](https://github.com/jmbekd/Developing_Data_Projects/).

--- &twocol w1:35% w2:65%

## Non-Normality and Inflection Points 

*** =left 

### Non-Normality

Distributions of concentration data for many analytes are rarely normally distributed.
- Can transform the data set to make approximately normal.
- Concentration data for analytes of environmental concern or minerals of economic significance commonly approximate [lognormal distributions](http://en.wikipedia.org/wiki/Log-normal_distribution) and hence, can be modeled as normal distributions by log-transforming the data.

*** =right 

### Inflection Points

For simple mixtures, histograms and density plots of the data may be used to graphically determine the population parameters for each subpopulation. 
- For more complex mixtures, it is often easier to estimate the relative proportions of each subpopulation from the [inflection points](http://en.wikipedia.org/wiki/Inflection_point) (change in direction of curvature) on a [QQ plot](http://en.wikipedia.org/wiki/Q%E2%80%93Q_plot) of the data and to partition the data into component populations using this/these estimate(s).

<img src="assets/fig/unnamed-chunk-1-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" style="display: block; margin: auto;" />


