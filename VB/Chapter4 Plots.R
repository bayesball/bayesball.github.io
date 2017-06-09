## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=8, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(readr)
library(ggplot2)
library(dplyr)

## ------------------------------------------------------------------------
d <- read_csv("https://bayesball.github.io/VB/data/count2015a.csv")
head(d)

## ------------------------------------------------------------------------
ggplot(d, aes(N.Pitches, Runs, label=count)) +
  geom_point() +
  geom_path(data=filter(d, strikes==0),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, strikes==1),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, strikes==2),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==0),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==1),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==2),
     aes(N.Pitches, Runs), color="blue") +
  geom_path(data=filter(d, balls==3),
     aes(N.Pitches, Runs), color="blue") +
  xlab("Pitch Number") +
  ylab("Runs Value") +
  ggtitle("") +
  geom_hline(yintercept=0, color="black") +
  geom_label()

## ------------------------------------------------------------------------
S <- read_csv("https://bayesball.github.io/VB/data/count2015b.csv")
head(S)

## ------------------------------------------------------------------------
ggplot(S, aes(N.Pitches, Runs, label=count, size=N)) +
  xlab("Number of Pitch") +
  ylab("Runs Value") +
  geom_hline(yintercept=0, color="black") +
  geom_label()

