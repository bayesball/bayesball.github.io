## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=8, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(Lahman)
library(dplyr)
library(ggplot2)

## ------------------------------------------------------------------------
select(sample_n(Teams, 8), yearID, teamID, HR, G)

## ------------------------------------------------------------------------
S <- summarize(group_by(Teams, yearID),
               HR = sum(HR),
               G = sum(G))

## ------------------------------------------------------------------------
S2 <- filter(S, yearID >= 1900)

## ------------------------------------------------------------------------
ggplot(S2, aes(yearID, HR / G)) +
  geom_point() +
  geom_smooth(method="loess", span=0.3, se=FALSE)

## ------------------------------------------------------------------------
S <- summarize(group_by(Batting, yearID, playerID),
                 HR=sum(HR, na.rm=TRUE))

## ------------------------------------------------------------------------
S1 <- summarize(group_by(S, yearID),
                 HR=max(HR, na.rm=TRUE))
S2 <- filter(S1, yearID >= 1900)

## ------------------------------------------------------------------------
head(S2)

## ------------------------------------------------------------------------
ggplot(S2, aes(yearID, HR)) +
    geom_point() +
    geom_smooth(method="loess", span=0.3, se=FALSE)

