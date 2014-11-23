---
title       : Determining Population Parameters for Mixtures of Normal Distributions
subtitle    : 
author      : John Montgomery-Brown
job         : Coursera
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Determining Population Parameters for Mixtures of Normal Distributions

1. In many environmental applications (environmental pollution, etc.) the collected data represent a mixture of observations of different populations (e.g., a background population and a contaminated population, e.g., uncontaminated soil that contains a variety of metals (e.g., zinc, copper, arsenic, etc.) at background levels and a contaminated soil that contains all of the compounds in the uncontaminated soil albeit, some at much higher concentrations than those observed in the uncontaminated materials.)
2. The distributions of contaminant concentration data for contaminated sites are not typically normally distributed, they tend to be positively skewed and are usually modeled using a lognormal distribution.
3. The use of a maximum likelihood estimation using an expectation-maximization algoriths can be used to determine the population parameters for each population^1 in the mixture.

^1 Many populations can be transformed such that they are approximately normal. For example, a lognormal distribution (common in environmental applications) can be transformed to a normal population by taking the logarithm of the data.

Plot 4 panel plot of a single population, non-overlapping, slightly overlapping, high degree of overlapping populations. Maybe with different standard deviations?

--- .class #id 

## QQ Plots

Using QQ Plots, or cumulative frequency plots, one can determine the approximate composition of a mixture of normal distributions. In a QQ Plot, a normal distribution plots as a straight line, with the slope being proportional to the standard deviation of the population and the intercept the mean of the population. When you have a 50:50 mixture of two normally distributed populations, or a mixture of three normally distributed populations as shown in the figure below, the resulting plot looks like an S... Such mixtures produce curved patterns on QQ Plots.  Similar patterns are commonly interpreted to consist of two or more overlapping subpopulations. The central parto f hte curve contains an inflection point (change in direction of curvature), indicating the possible presence of multiple normal subpopulations, In theory, there is one more subpopulation than there are inflection points, however, sampling and analytical error generally limit the recognition of more than three or four subpopulations in a data set.

Plot 4 panel plot showing individual populations, non-overlapping mixture, slightly overlapping mixture of two, and highly overlapping mixture of three populations?

--- .class #id 

## Partitioning

Partitioning is the term applied to procedures to separate the cumulative curve of a mixture of subpopulations into the component subpopulations. By identifying the inflection points in a QQ Plot of the normal data, we can partition the mixture of subpopulations into its component subpopulations. This is accomplished using the `normalmixEM` function in the `mixtools` package which uses maximum-likelihood estimation procedures on an [expectation-maximization](http://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm) algorithm, to determine the population parameters of the component subpopulations within the mixture.

--- .class #id 

## Introduction to Application

XXXX

--- .class #id 

## Embed the Shiny Application

Maybe.

[Application](https://jmbekd.shinyapps.io/Developing_Data_Projects/)

--- .class #id 
