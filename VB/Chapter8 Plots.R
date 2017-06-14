## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)

## ------------------------------------------------------------------------
d <- read_csv("https://bayesball.github.io/VB/data/WSGame7.csv")
d$Play_Number <- 1:dim(d)[1]
d$WE  <- as.numeric(str_replace(d$WE, "%", ""))
head(d)

## ------------------------------------------------------------------------
ggplot(d, aes(Play_Number, WE / 100)) +
  geom_point(size=2) +
  geom_line() +
  ylim(0, 1) +
  ggtitle("") +
  ylab("Probability Indians Win") +
  geom_hline(yintercept = .50, color="blue", size=1.5) +
  annotate("text", x=cumsum(c(0, 10, 7, 9, 9, 12, 8,
                              8, 10, 8)) +
             c(10, 7, 9, 9, 12, 8,
               8, 10, 8, 14) / 2,
           y=0.90,
           label = as.character(1:10), size=5) +
  annotate("text", x=45, y=0.98, 
           label="INNING", size=6) +
  xlab("Play Number")

## ------------------------------------------------------------------------
ggplot(d, aes(Play_Number, LI)) +
  geom_segment(aes(xend = Play_Number, yend = 0),
               size = 2, lineend = "butt") +
  xlab("Play Number") +
  ylab("Leverage")  +
  ylim(0, 5.8) +
  annotate("text", x=cumsum(c(0, 10, 7, 9, 9, 12, 8,
                              8, 10, 8)) +
             c(10, 7, 9, 9, 12, 8,
               8, 10, 8, 14) / 2,
           y=5,
           label = as.character(1:10), size=5) +
  annotate("text", x=45, y=5.5, label="INNING", size=6) 

## ------------------------------------------------------------------------
ggplot(d, aes(Play_Number, WPA)) +
  geom_segment(aes(xend = Play_Number, yend = 0),
               size = 2, lineend = "butt") +
  xlab("Play Number") +
  ylab("Win Probability Added") +
  ylim(-0.24, 0.6) +
  annotate("text", x=cumsum(c(0, 10, 7, 9, 9, 12, 8,
                              8, 10, 8)) +
             c(10, 7, 9, 9, 12, 8,
               8, 10, 8, 14) / 2,
           y=0.53,
           label = as.character(1:10), size=5) +
  annotate("text", x=45, y=0.60, label="INNING", size=6) +
  annotate('text', x=71, y=0.45, label="Davis\nHR") +
  annotate('text', x=85, y=0.38, label="Zobrist\n2B") +
  annotate('text', x=77, y=-0.22, label="Baez\nSO") 

