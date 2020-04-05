## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE, message = FALSE)


## ------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(readr)


## ------------------------------------------------------------------------
source("http://www-math.bgsu.edu/~albert/ACTION/zone.R")


## ------------------------------------------------------------------------
tribe <- read_csv("http://bit.ly/tribepitching")


## ------------------------------------------------------------------------
tribe %>% 
  filter((description %in% 
          c("hit_by_pitch", "pitchout")) == FALSE) %>% 
  mutate(Type = ifelse(description %in%
        c("ball", "blocked_ball", 
          "called_strike"), "Called", "Swing")) -> tribe


## ------------------------------------------------------------------------
ck <- filter(tribe, player_name == "Corey Kluber")


## ------------------------------------------------------------------------
ck %>% 
  group_by(pitch_type) %>% 
  summarize(N = n()) %>% 
  ggplot(aes(pitch_type, N)) +
  geom_col()


## ------------------------------------------------------------------------
ck %>% 
  ggplot(aes(pitch_type, release_speed)) +
  geom_boxplot()


## ------------------------------------------------------------------------
ck %>% 
  ggplot(aes(pfx_x, pfx_z, color = pitch_type)) +
  geom_point()


## ------------------------------------------------------------------------
twopitches <- filter(ck,
                     pitch_type %in% c("CU", "FC"))


## ------------------------------------------------------------------------
twopitches %>% 
  ggplot(aes(plate_x, plate_z)) +
  geom_point() +
  facet_wrap(~ pitch_type) +
  add_zone()


## ------------------------------------------------------------------------
ck %>% filter(Type == "Called") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


## ------------------------------------------------------------------------
ck %>% filter(Type == "Swing") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


## ------------------------------------------------------------------------
ck %>% filter(Type == "Swing", pitch_type == "CU") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


## ------------------------------------------------------------------------
ck %>% filter(Type == "Swing", pitch_type == "FC") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone()


## ------------------------------------------------------------------------
kluber_bauer <- filter(tribe, player_name %in%
   c("Corey Kluber", "Trevor Bauer"))


## ------------------------------------------------------------------------
kluber_bauer %>% 
  filter(Type == "Swing", pitch_type == "CH") %>% 
  ggplot(aes(plate_x, plate_z, color = description)) +
  geom_point() +
  add_zone() +
  facet_grid(~ player_name)


## ------------------------------------------------------------------------
kluber_bauer %>% 
  mutate(Miss = ifelse(description %in%
  c("swinging_strike", "swinging_strike_blocked"),
  TRUE, FALSE)) -> kluber_bauer


## ------------------------------------------------------------------------
kluber_bauer %>% 
  filter(Type == "Swing") %>% 
  group_by(player_name, Miss) %>% 
  count()

