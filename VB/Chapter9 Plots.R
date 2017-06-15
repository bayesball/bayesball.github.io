## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(BayesTestStreak)
library(gridExtra)

## ------------------------------------------------------------------------
mavg_plot

## ------------------------------------------------------------------------
walker_id <- find_id("Neil Walker")
aoki_id <- find_id("Nori Aoki")

## ------------------------------------------------------------------------
walker <- streak_data(walker_id, pbp2016, "H", AB=TRUE)
aoki <- streak_data(aoki_id, pbp2016, "H", AB=TRUE)

## ------------------------------------------------------------------------
plot_streak_data(walker) + theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Walker")

## ------------------------------------------------------------------------
plot_streak_data(aoki) + theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Aoki")

## ------------------------------------------------------------------------
mavg_plot(walker, 50) +  theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Walker")

## ------------------------------------------------------------------------
mavg_plot(aoki, 50) + theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Aoki")

## ------------------------------------------------------------------------
p1 <- mavg_plot(walker, 50) + ylim(.1, .5) +
  annotate("text", x=200, y=0.45,
           label="Neil Walker", size=7) +
  ylab("Moving Average") + xlab("") 
p2 <- mavg_plot(aoki, 50) + ylim(.1, .5) +
  annotate("text", x=200, y=0.45,
           label="Nori Aoki", size=7) +
  ylab("Moving Average") + xlab("At Bat Number")
grid.arrange(p1, p2)


## ------------------------------------------------------------------------
sp <- find.spacings(walker)
geometric.plot(sp$y) + theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Walker")

## ------------------------------------------------------------------------
sp <- find.spacings(aoki)
geometric.plot(sp$y) + theme(plot.title = element_text(colour = "blue", size = 18, 
        hjust = 0.5)) + ggtitle("Aoki")

