## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=8, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(readr)
library(knitr)
library(ggplot2)

## ------------------------------------------------------------------------
RR <- read_csv("https://bayesball.github.io/VB/runs2015.csv")

## ------------------------------------------------------------------------
kable(RR)

## ------------------------------------------------------------------------
ggplot(RR, aes(Bases, Mean, label=O)) +
    geom_point(size=3) + 
    geom_label(color="black", size=4,
               fontface="bold") +
    ylab("Runs Scored in \n Remainder of Inning") +
    xlab("Runners on Base") +
    theme(axis.text = element_text(size=16),
          axis.title = element_text(size=16))

