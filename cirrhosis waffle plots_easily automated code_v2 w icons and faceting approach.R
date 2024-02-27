install.packages("dplyr","magrittr","ggplot2","waffle","socviz","hrbrthemes","fontawesome", "cowplot")
library(dplyr, magrittr, ggplot2, waffle, socviz, hrbrthemes, fontawesome, cowplot)
library(waffle)

##Create data

data.frame(suboutcomes = c("normal surveillance",
                           "false alarms without biopsies",
                           "false alarms with biopsies",
                           "survive regardless of surveillance",
                           "deaths averted",
                           "die despite surveillance",
                           "deaths from other causes"),
           vals = c(654,111,39,28,13,69,82),
           outcomes=c("normal surveillance",
                      "false alarm",
                      "false alarm",
                      "HCC diagnosis",
                      "HCC diagnosis",
                      "HCC diagnosis",
                      "deaths from other causes")
) -> xdf

outcome_vals<-xdf %>%
  group_by(outcomes) %>% 
  summarise(outcome_total = sum(vals))

xdf<- xdf %>% 
  left_join(outcome_vals, by = "outcomes") %>% 
  mutate(outcomes_new=paste(outcomes," (",outcome_total,")"),
         suboutcomes_new=paste(vals, suboutcomes))

##faceting approach
 xdf<-xdf %>% 
   group_by(outcomes) %>% 
   mutate(outcomes_new=paste(suboutcomes_new,collapse="\n")) 

 p<- xdf %>%
  count(suboutcomes, outcomes_new, wt = vals) %>%
  ggplot(
    aes(fill = suboutcomes, values = n)
  ) +
  geom_waffle(
    n_rows = 10,
    size = 0.33, 
    colour = "white",
    ##flip = TRUE,
    show.legend = TRUE
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  facet_wrap(~outcomes_new,
             nrow=4)
p


##subset data
xdf1<-filter(xdf, outcomes=="normal surveillance")
xdf2<-filter(xdf, outcomes=="false alarm")
xdf3<-filter(xdf, outcomes=="HCC diagnosis")
xdf4<-filter(xdf, outcomes=="deaths from other causes")

##make titles 
title1<-paste(sum(xdf1$vals),"will have normal surveillance")
title2<-paste(sum(xdf2$vals),"will have at least one false alarm")
title3<-paste(sum(xdf3$vals),"will be diagnosed with HCC")
title4<-paste(sum(xdf4$vals),"deaths from other causes")

##plot waffles

p1<- xdf1 %>%
  count(outcomes_new, suboutcomes, wt = vals) %>%
  ggplot(
    aes(fill = outcomes_new, values = n)
  ) +
  geom_waffle(
    n_rows = 20,
    size = 0.33, 
    colour = "white",
    flip = TRUE,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#969696")
  ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title1)+
  # facet_wrap(~suboutcomes,
  #            ##labeller = labeller(group = p2labs),
  #            nrow=1
  # )+
  ##geom_text(nudge_y = 1) +
  theme(panel.spacing.x = unit(0, "npc")) +
  theme(strip.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5))+ 
  scale_colour_grey()
p1

p2<- xdf2 %>%
  count(outcomes_new, suboutcomes, wt = vals) %>%
  ggplot(
    aes(fill = outcomes_new, values = n)
  ) +
  geom_waffle(
    n_rows = 20,
    size = 0.33, 
    colour = "white",
    flip = TRUE,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#969696")
  ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title2)+
  facet_wrap(~suboutcomes,
             ##labeller = labeller(group = p2labs),
             nrow=1
  )+
  ##geom_text(nudge_y = 1) +
  theme(panel.spacing.x = unit(0, "npc")) +
  theme(strip.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5))+ 
  scale_colour_grey()

p3<- xdf3 %>%
  count(outcomes_new, suboutcomes, wt = vals) %>%
  ggplot(
    aes(fill = outcomes_new, values = n)
  ) +
  geom_waffle(
    n_rows = 20,
    size = 0.33, 
    colour = "white",
    flip = TRUE,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#969696")
  ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title3)+
  facet_wrap(~suboutcomes,
             ##labeller = labeller(group = p3labs),
             nrow=1
  )+
  ##geom_text(nudge_y = 1) +
  theme(panel.spacing.x = unit(0, "npc")) +
  theme(strip.text.x = element_text(hjust = 0.5),
        plot.title = element_text(hjust = 0.5))+ 
  scale_colour_grey()

p4<- xdf4 %>%
  count(outcomes_new, suboutcomes, wt = vals) %>%
  ggplot(
    aes(fill = outcomes_new, values = n)
  ) +
  geom_waffle(
    n_rows = 20,
    size = 0.33,
    colour = "white",
    flip = TRUE,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("#969696")
  ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title4)+ 
  scale_colour_grey()

##arrange waffles on page, adjusting ratio of width and height each waffle occupies

lay_out(list(p1,1:100,1:28),list(p2,1:36,29:100),list(p3,37:70,29:100),list(p4,71:100,29:100))

#experimenting with pictograms

install.packages("emojifont")
library(emojifont)
load.fontawesome()

p1<- xdf1 %>% ggplot(aes(label = outcomes)) +
  geom_pictogram(
    n_rows = 25, aes(color = outcomes, values = vals),
    family = "fontawesome-webfont",
    flip = TRUE,
    size=3,
    show.legend = FALSE
  ) +
  scale_label_pictogram(
    name = "Case",
    values = c("user")
  ) +
  scale_color_manual(
    name = "Case",
    values = c("grey85")
   ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title1)+
  theme(strip.text = element_text(
    size = 10.5),
    plot.title = element_text(hjust = 0.5))

p2<- xdf2 %>% ggplot(aes(label = outcomes)) +
  geom_pictogram(
    n_rows = 25, aes(color = outcomes, values = vals),
    family = "fontawesome-webfont",
    flip = TRUE,
    size=3,
    show.legend = FALSE
  ) +
  scale_label_pictogram(
    name = "Case",
    values = c("user")
  ) +
  scale_color_manual(
    name = "Case",
    values = c("grey85")
  ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title2)+
  facet_wrap(~suboutcomes,
             ##labeller = labeller(group = p2labs),
             nrow=1
  )+
  theme(strip.text = element_text(size = 10.5),
        plot.title = element_text(hjust = 0.5))

p3<- xdf3 %>% ggplot(aes(label = outcomes)) +
  geom_pictogram(
    n_rows = 25, aes(color = outcomes, values = vals),
    family = "fontawesome-webfont",
    flip = TRUE,
    size=3,
    show.legend = FALSE
  ) +
  scale_label_pictogram(
    name = "Case",
    values = c("user")
  ) +
  scale_color_manual(
    name = "Case",
    values = c("grey85")
  ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title3)+
  facet_wrap(~suboutcomes,
             ##labeller = labeller(group = p2labs),
             nrow=1
  )+
  theme(strip.text = element_text(
    size = 10.5),
    plot.title = element_text(hjust = 0.5))

p4<- xdf4 %>% ggplot(aes(label = outcomes)) +
  geom_pictogram(
    n_rows = 25, aes(color = outcomes, values = vals),
    family = "fontawesome-webfont",
    flip = TRUE,
    size=3,
    show.legend = FALSE
  ) +
  scale_label_pictogram(
    name = "Case",
    values = c("user")
  ) +
  scale_color_manual(
    name = "Case",
    values = c("grey85")
  ) +
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle()+
  ggtitle(title4)+
  theme(strip.text = element_text(
    size = 10.5),
    plot.title = element_text(hjust = 0.5))
# +
#   facet_wrap(~suboutcomes,
#              ##labeller = labeller(group = p2labs),
#              nrow=1
#   )


right_col<-plot_grid(p2,p3,p4,ncol=1,rel_heights = c(29, 47,24))
plot_grid(p1,right_col,rel_widths=c(30,70))
