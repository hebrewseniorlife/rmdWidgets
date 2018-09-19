# rmdWidgets

The goal of rmdWidgets is to make it easy to insert document widgets to a rmarkdown HTML/LaTeX document. 

## Installation

``` r
devtools::install_github("hebrewseniorlife/rmdWidgets")
```

## Example

This is a basic example which shows you how to solve a common problem:

````
---
title: "Untitled"
author: "Hao"
date: "9/19/2018"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(rmdWidgets)
```

```{r}
rmd_checkbox(c("Yes", "No"), selected = 1, label = "Hello?")
```
````

