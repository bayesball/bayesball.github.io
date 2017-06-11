## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, 
                      fig.height=5, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)

## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)

## ------------------------------------------------------------------------
library(readr)
CK <- read_csv("https://bayesball.github.io/VB/data/kershaw2016.csv")
head(CK)

## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)

## ------------------------------------------------------------------------
S_CK <- filter(summarize(group_by(CK, pitch_type),
                  N=n()),
            pitch_type %in% c("SL", "FF", "CU", "CH"))
ggplot(S_CK, aes(pitch_type, N)) +
  geom_point(size=3, color="blue") +
  coord_flip() +
  ggtitle("Frequencies of Pitch Type of Clayton Kershaw") +
  theme(plot.title = element_text(size = 14,
                hjust = 0.5))

## ------------------------------------------------------------------------
ggplot(filter(CK, pitch_type %in%
                c("SL", "FF", "CU", "CH")),
       aes(pitch_type, start_speed)) +
  geom_boxplot() + coord_flip() +
  ggtitle("Pitch Speeds") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5)) +
     ylim(70, 100)

## ------------------------------------------------------------------------
CK <- filter(CK, pitch_type %in% c("CU",
                          "FF", "SL"))
ggplot(CK,
  aes(pfx_x, pfx_z, shape=pitch_type)) +
  geom_point(color="blue", size=2, alpha=0.5) +
  ggtitle("Pitch Breaks") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5)) +
  xlab("Horizontal Break") + ylab("Vertical Break")

## ------------------------------------------------------------------------
topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.85
outKzone <- 0.85
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)
ggplot(CK) +
  geom_point(data= filter(CK, pitch_type=="CU"),
             aes(px, pz), shape=1) +
  geom_point(data= filter(CK, pitch_type=="FF"),
             aes(px, pz), shape=2) +
  geom_point(data= filter(CK, pitch_type=="SL"),
             aes(px, pz), shape=3) +
  geom_path(aes(x, y), data=kZone, lwd=1, col="blue") +
  facet_wrap(~ pitch_type, ncol=2) +
  xlim(-2, 2) + ylim(-0.5, 5) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black")) +
  ggtitle("Pitch Locations") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))

## ------------------------------------------------------------------------
ggplot(CK) +
  geom_density_2d(aes(px, pz), color="black") +
  geom_path(aes(x, y), data=kZone, lwd=1, col="blue") +
  facet_wrap(~ pitch_type, ncol=2) +
  xlim(-2, 2) + ylim(-0.5, 5) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black")) +
  ggtitle("Pitch Locations") +
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5))

## ------------------------------------------------------------------------
SO <- summarize(group_by(CK, pitch_type, des), N=n())
SO <- mutate(SO,
      Outcome=ifelse(str_detect(des, "Foul") == TRUE, "Foul",
      ifelse(str_detect(des, "Swing") == TRUE |
               des == "Missed Bunt", "Swing and Miss",
      ifelse(str_detect(des, "Ball") == TRUE, "Ball",
      ifelse(str_detect(des, "In play") == TRUE, "In play",
             des)))))
SOS <- summarize(group_by(SO, pitch_type, Outcome),
                 F=sum(N))
SOS1 <- summarize(group_by(SO, pitch_type),
                 Total=sum(N))
inner_join(SOS, SOS1) %>%
  mutate(Percentage = 100 * F / Total) -> SOS
ggplot(SOS,
        aes(Outcome, Percentage)) +
  geom_point(size=3, color="blue") +
  coord_flip() + facet_wrap(~ pitch_type, ncol=1) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black"))

## ------------------------------------------------------------------------
CK <- mutate(CK,
             Foul = str_detect(des, "Foul"),
             InPlay = str_detect(des, "In play"),
             Miss = str_detect(des, "Swing"),
             Swing = Foul | InPlay | Miss)
CK_swing <- filter(CK, Swing == TRUE)
ggplot(CK_swing, aes(px, pz, color=Miss)) +
  geom_point(alpha=0.75) +
  facet_wrap(~ pitch_type, ncol=2) +
  geom_path(aes(x, y), data=kZone, lwd=1, col="black") +
  facet_wrap(~ pitch_type, ncol=2) +
  xlim(-2, 2) + ylim(-0.5, 5) +
  scale_colour_manual(values = c("gray60", "blue")) +
  theme(strip.text = element_text(size = rel(1.5),
                                  hjust=0.5,
                                  color = "black"))

