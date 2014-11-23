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

1. In many environmental applications (environmental pollution, etc.) the collected data represent a mixture of observations of different populations (e.g., a background population and a contaminated population, e.g., uncontaminated soil that contains a variety of metals (e.g., zinx, copper, arsenic, etc.) at background levels and a contaminated soil that contains all of the compounds in the uncontaminated soil albeit, some at much higher concentrations than those observed in the uncontaminated materials.)
2. The distributions of contaminant concentration data for contaminated sites are not typically normally distributed, they tend to be positively skewed and are usually modeled using a lognormal distribution.
3. The use of a maximum likelihood estimation using an expectation-maximization algoriths can be used to determine the population parameters for each population^1 in the mixture.

^1 Many populations can be transformed such that they are approximately normal. For example, a lognormal distribution (common in environmental applications) can be transformed to a normal population by taking the logarithm of the data.

--- .class #id 

## Slide 2

Using QQ Plots, or cumulative frequency plots, one can determine the approximate composition of a mixture of 


