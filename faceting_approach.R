install.packages("pacman")
library(pacman)
p_load(dplyr, magrittr, ggplot2, waffle, socviz, hrbrthemes, fontawesome, cowplot)


##Create data

data.frame(suboutcomes = c("undergo normal surveillance",
                           "false alarms without biopsies",
                           "false alarms with biopsies",
                           "deaths averted",
                           "survive regardless of surveillance",
                           "die despite surveillance",
                           "deaths from other causes"),
           vals = c(654,111,39,28,13,69,82),
           outcomes=c("Normal surveillance",
                      "False alarm",
                      "False alarm",
                      "HCC diagnosis",
                      "HCC diagnosis",
                      "HCC diagnosis",
                      "Deaths from other causes")
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
  mutate(outcomes_new=paste(suboutcomes_new,collapse="\n"),
         rank=row_number()) %>% 
  ungroup() %>% 
  mutate(outcomes_new=paste(outcomes,"\n","\n",outcomes_new))

p<- xdf %>%
  count(factor(suboutcomes,levels = c("undergo normal surveillance",
                                             "false alarms without biopsies",
                                             "false alarms with biopsies",
                                             "deaths averted",
                                              "survive regardless of surveillance",
                                             "die despite surveillance",
                                             "deaths from other causes")),
        outcomes, suboutcomes_new, wt = vals,rank, outcome_total) %>%
  ggplot(
    aes(fill = suboutcomes_new, values = n)
  ) +
  geom_waffle(
    n_rows = 25,
    size = 0.33, 
    colour = "white",
    flip = TRUE,
    show.legend = FALSE
  ) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  facet_wrap(~outcomes,
             nrow=1)+
  theme(legend.position="none")+
  theme(strip.text.x = element_text(vjust = 1,size=20))+
  geom_label(aes(
    ##colour=suboutcomes_new,
    x=1,y=outcome_total/25+1+2*rank,hjust=0
    ,label=suboutcomes_new
    ,size=5))

p