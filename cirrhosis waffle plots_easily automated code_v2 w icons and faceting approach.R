library(pacman)
p_load(here,
       tidyverse,
       magrittr,
       waffle,
       socviz,
       hrbrthemes,
       fontawesome,
       cowplot,
       glue,
       forcats)

fnPad <- function(n, l = 4) {(trunc((n-1) / w)+1) * w - n + w*l}

##Create data
yrs <- 5
w <- 25

outc <- c("{outcome_total} will have normal surveillance\nfor all {yrs} years",
          "{outcome_total} will have at least 1 false alarm\nduring the {yrs} years",
          "{outcome_total} will die from other causes\nduring the {yrs} years",
          "{outcome_total} will receive a diagnosis of HCC\nduring the {yrs} years")

cats <- c("normal surveillance",
          "false alarms without biopsies",
          "spacerFA",
          "false alarms with biopsies",
          "deaths from other causes",
          "survive regardless of surveillance",
          "spacer1",
          "deaths averted",
          "spacer2",
          "die despite surveillance")

lstIn <- list(nNorm      = 654,
              nFA.noBx   = 111,
              nFA.Bx     = 39,
              nDieOth    = 82,
              nHCC.surv  = 28,
              nHCC.avert = 13,
              nHCC.die   = 69)

lstIn %<>% 
  append(fnPad(lstIn$nFA.noBx), 2) %>% 
  append(fnPad(lstIn$nHCC.surv), 6) %>% 
  append(fnPad(lstIn$nHCC.avert), 8)

data.frame(suboutcomes = factor(cats,
                                levels = cats),
           vals        = as.numeric(lstIn),
           outcomes    = factor(c(outc[1],
                                  rep(outc[2], 3),
                                  outc[3],
                                  rep(outc[4], 5)),
                                levels = outc)
) -> xdf

outcome_vals<-xdf %>%
  group_by(outcomes) %>% 
  summarise(outcome_total = sum(ifelse(str_starts(suboutcomes, "space"), 0, vals)))

xdf<- xdf %>% 
  left_join(outcome_vals, by = "outcomes") %>% 
  rowwise() %>% 
  mutate(outcomes_new = glue(as.character(outcomes)),
         suboutcomes_new=paste(vals, suboutcomes))

##faceting approach

p<- xdf %>%
  arrange(suboutcomes) %>% 
  group_by(outcomes) %>% 
  mutate(cumV = cumsum(vals),
         strSub = ifelse(str_detect(suboutcomes, "desp|avert|regard|th b"), suboutcomes_new, "")) %>% 
  ggplot(
    aes(fill = suboutcomes, values = vals)
  ) +
  geom_waffle(
    n_rows = w,
    size = 0.3, 
    colour = "white",
    flip = TRUE,
    show.legend = FALSE
  ) +
  geom_text(aes(x = 0.5, y = ceiling(cumV/w)+1.5, label = strSub), hjust = 0, size = 12 / .pt) +
  scale_fill_manual(breaks = cats,
                    values = c("blue",
                               "pink",
                               "white",
                               "red",
                               "black",
                               "darkgreen",
                               "white",
                               "cyan",
                               "white",
                               "black")) +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  facet_wrap(~fct_inorder(outcomes_new),
             nrow = 2, 
             # labeller = label_wrap_gen(multi_line = TRUE)
             )+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(strip.text = element_text(size = 12, family = "sans", lineheight = 1, face = "bold", margin = margin(0,0,0.5,0,"mm")),
        # panel.spacing = ,
        plot.margin = margin()) 

p

ggsave(here("waffle.jpg"), width = 20, height = 20, units = "cm", dpi = 500)
