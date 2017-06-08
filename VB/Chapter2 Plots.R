## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=8, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
plot_hr_trajectory <- function(playername){
  require(Lahman)
  require(dplyr)
  require(stringr)
  require(ggplot2)
  names <- unlist(str_split(playername, " "))
  info <- filter(Master, nameLast==names[2],
                       nameFirst==names[1])

  bdata <- filter(Batting, playerID==info$playerID)
  bdata <- mutate(bdata,
          birthyear = ifelse(info$birthMonth >= 7, 
                  info$birthYear + 1, info$birthYear),
          Age = yearID - birthyear)

  ggplot(bdata, aes(yearID, HR / AB)) + 
    geom_point() +
    geom_smooth(method="loess", se=FALSE)
}

## ------------------------------------------------------------------------
p1 <- plot_hr_trajectory("Mickey Mantle")
p1

## ------------------------------------------------------------------------
p2 <- plot_hr_trajectory("Mike Schmidt")
p2

## ------------------------------------------------------------------------
ggplot(rbind(p1$data, p2$data), aes(Age, HR / AB)) +
  geom_point() +
  geom_smooth(method="loess", se=FALSE) +
  facet_wrap(~ playerID, ncol=1)

