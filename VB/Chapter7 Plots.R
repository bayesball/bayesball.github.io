## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=8, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

## ------------------------------------------------------------------------
d1 <- read_csv("https://bayesball.github.io/VB/data/Dashboard_2016.csv")
d2 <- read_csv("https://bayesball.github.io/VB/data/Plate_Discipline_2016.csv")
d <- inner_join(d1, d2, by="playerid")
vars <- c(14, 25:33)
d_subset <- d[, vars]
names(d_subset) <- c("OBP", "O_Swing", "Z_Swing", "Swing",
                     "O_Contact", "Z_Contact",
                     "Contact", "Zone",
                     "F_Strike", "SwStr")
names(d)[c(14, 25:33)] <- names(d_subset)

## ------------------------------------------------------------------------
ggplot(d, aes(Swing, Contact)) +
  geom_point(size=2) +
  geom_smooth(se=FALSE) +
  xlab("Swing Rate") + ylab("Contact Rate")

## ------------------------------------------------------------------------
d$K_Rate <- with(d, ifelse(K > .1875, "HI", "LO"))
d$y <- ifelse(d$K_Rate=="HI", 1, 0)
glm(y ~ Contact + Swing, data=d, family=binomial) -> F
ggplot(d, aes(Swing, Contact, 
              color=K_Rate)) +
  geom_point(size=3) +
  xlab("Swing Rate") + ylab("Contact Rate") +
  geom_abline(intercept = coef(F)[1] / (-coef(F)[2]),
              slope = coef(F)[3] / (-coef(F)[2])) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("black", "grey60"))

## ------------------------------------------------------------------------
d$BB_Cat <- with(d, ifelse(BB > .082, "HI", "LO"))
d$y <- ifelse(d$BB_Cat=="HI", 1, 0)
glm(y ~ Contact + Swing, data=d, family=binomial) -> F
ggplot(d, aes(Swing, Contact, 
              color=BB_Cat)) +
  xlab("Swing Rate") + ylab("Contact Rate") +
  geom_point(size=3) +
  geom_abline(intercept = coef(F)[1] / (-coef(F)[2]),
              slope = coef(F)[3] / (-coef(F)[2])) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("black", "grey60"))

## ------------------------------------------------------------------------
d <- mutate(d,
            K_Type=ifelse(K < .12, "TOP",
                  ifelse(K > .25, "BOTTOM", NA)))
select(filter(d, K_Type == "TOP"),
       Name.x, Team.x, K)
select(filter(d, K_Type == "BOTTOM"),
       Name.x, Team.x, K)

## ------------------------------------------------------------------------
d <- mutate(d,
            BB_Type=ifelse(BB > .13, "TOP",
                  ifelse(BB < .05, "BOTTOM", NA)))
select(filter(d, BB_Type == "TOP"),
       Name.x, Team.x, BB)
select(filter(d, BB_Type == "BOTTOM"),
       Name.x, Team.x, BB)

## ------------------------------------------------------------------------
ggplot(filter(d, K_Type %in% c("TOP", "BOTTOM")),
        aes(Z_Contact, O_Contact, color=K_Type)) +
         geom_point(size=3) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("grey50", "black" ))

## ------------------------------------------------------------------------
ggplot(filter(d, BB_Type %in% c("TOP", "BOTTOM")),
        aes(Z_Swing, O_Swing, color=BB_Type)) +
         geom_point(size=3) +
  scale_shape(solid = FALSE) +
  scale_colour_manual(values = c("grey50", "black" )) 

