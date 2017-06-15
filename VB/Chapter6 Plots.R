## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(Lahman)
library(readr)

## ------------------------------------------------------------------------
d <- read_csv("https://bayesball.github.io/VB/data/homeruns.csv")
head(d)

## ------------------------------------------------------------------------
ggplot(d, aes(180 - Horiz_Angle)) +
  geom_density() +
  xlim(30, 180 - 30) +
  xlab("Horizontal Angle") +
  ylab("Density") +
  geom_vline(xintercept=90) +
  annotate("text", x=40, y=0.015,
           label="Left\nField", size=6) +
  annotate("text", x=140, y=0.015,
           label="Right\nField", size=6)

## ------------------------------------------------------------------------
ggplot(d, aes(180 - Horiz_Angle, True_Dist)) +
  geom_point(alpha=0.1) + geom_smooth() +
  ylim(300, 500) + xlim(45, 130) +
  xlab("Horizontal Angle") +
  ylab("Distance")

## ------------------------------------------------------------------------
Names <- str_split(d$Hitter, ",")
one_row <- function(j, k)
  str_trim(Names[[j]][k])
d$LastName <- sapply(1:24299, one_row, 1)
d$FirstName <- sapply(1:24299, one_row, 2)
d2 <- inner_join(d,
                 select(Master, nameLast, nameFirst, bats),
                 by=c("LastName"="nameLast",
                      "FirstName"="nameFirst"))

## ------------------------------------------------------------------------
d2$Batting <- ifelse(d2$bats=="R",
                     "Right-Handed Hitter",
                     "Left-Handed Hitter")

## ------------------------------------------------------------------------
ggplot(filter(d2, bats=="R" | bats=="L"),
       aes(180 - Horiz_Angle)) +
  geom_density(size=1.0)  + xlim(45, 130) +
  xlab("Horizontal Angle") +
  ylab("Density") +
  facet_wrap(~ Batting, ncol=1) +
  theme(strip.text = element_text(face="bold", size=16))

## ------------------------------------------------------------------------
S <- summarise(group_by(d2, Ballpark),
               NL=sum(180 - Horiz_Angle < 90),
               NR=sum(180 - Horiz_Angle > 90),
               PL=NL / (NL + NR))

## ------------------------------------------------------------------------
ggplot(filter(S, NL + NR > 200), aes(Ballpark, PL)) +
  geom_point() + coord_flip() +
  ylab("Proportion of Home Runs to Left") +
  geom_hline(yintercept = 0.5)

## ------------------------------------------------------------------------
S200 <- filter(S, NL + NR > 200)
S200 <- arrange(S200, desc(PL))
Sextreme <- rbind(slice(S200, 1:8),
                  slice(S200, 28:31))
ballparks <- as.character(arrange(Sextreme, PL)$Ballpark)
d2$Ballpark <- factor(d2$Ballpark,
                      levels=ballparks)

## ------------------------------------------------------------------------
ggplot(filter(d2, bats=="R" | bats=="L",
              Ballpark %in% Sextreme$Ballpark),
       aes(180 - Horiz_Angle)) +
  geom_density() +
  facet_wrap(~ Ballpark, ncol=4) +
  geom_vline(xintercept = 90, color="blue") +
  xlab("Horizontal Angle") + ylab("Density")

