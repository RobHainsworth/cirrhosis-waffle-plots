library(pacman)
p_load(here,
       tidyverse,
       magrittr,
       waffle,
       hrbrthemes,
       glue,
       forcats)

fnPad <- function(n, l = 4) {(trunc((n-1) / w)+1) * w - n + w*l}

yrs <- 5
w <- 25

tblX <- tribble(
  ~col, ~grp,  ~nn,        ~cat,                                                          ~label,   ~face,          ~fll,   ~ht,
  1L,   2L,  82L,    "DieOth",       "{nnn} will die from other causes\nduring the {yrs} years",  "bold",       "black",  2.5,
  1L,   2L,  -6L,     "head2",                                                              NA, "plain",       "white",    NA,
  1L,   1L, 654L,      "Norm",                                                              NA, "plain",        "blue",    NA,
  1L,   1L,  -1L,     "head1",       "{nnn} will have normal surveillance\nfor all {yrs} years",  "bold",       "white",  1.5,
  2L,   3L,  28L,  "HCC.surv",                       "{nn} survive regardless of surveillance", "plain", "forestgreen",   1.5,
  2L,   3L,  -3L,     "spac2",                                                              NA, "plain",       "white",    NA,
  2L,   3L,  13L, "HCC.avert",                                           "{nn} deaths averted", "plain",        "cyan",   1.5,
  2L,   3L,  -3L,     "spac3",                                                              NA, "plain",       "white",    NA,
  2L,   3L,  69L,   "HCC.die",                                 "{nn} die despite surveillance", "plain",       "black",   1.5,
  2L,   3L,  -2L,     "head3",  "{nnn} will receive a diagnosis of HCC\nduring the {yrs} years",  "bold",       "white",  2.5,
  2L,   3L, -600,     "spac4",                                                              NA, "plain",       "white",    NA,
  2L,   4L, 111L,   "FA.noBx",                            "{nn} false alarms without biopsies", "plain",        "pink",   1.5,
  2L,   4L,  -3L,     "spac6",                                                              NA, "plain",       "white",    NA,
  2L,   4L,  39L,     "FA.Bx",                               "{nn} false alarms with biopsies", "plain",         "red",   1.5,
  2L,   4L,  -2L,     "head4", "{nnn} will have at least 1 false alarm\nduring the {yrs} years",  "bold",       "white",  2.5
)

tblX %<>% 
  group_by(grp) %>%
  mutate(nnn = cumsum(nn)) %>% 
  ungroup() %>% 
  mutate(nn = ifelse(nn<0, ifelse(nn<=-100, -nn/100*w, fnPad(lag(nn), -nn)), nn)) %>% 
  rowwise() %>% 
  mutate(str = glue(label, .na = "")) %>% 
  group_by(col) %>% 
  mutate(cumV = cumsum(nn))

tblX %>%
  ggplot(aes(fill = fll, values = nn)) +
  geom_waffle(n_rows = w,
              size = 0.3, 
              colour = "white",
              flip = TRUE,
              show.legend = FALSE) +
  geom_text(aes(x = 0.5, y = ceiling(cumV/w)+ht, label = str, fontface = face), hjust = 0, size = 12 / .pt, lineheight = 0.85) +
  scale_fill_identity() +
  coord_equal() +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  facet_grid(cols = vars(col))+
  scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme(strip.text = element_blank(),
        panel.spacing = unit(2, "line"),
        plot.margin = margin(1, 1, 1, 1, "line"))

ggsave(here("waffle.jpg"), width = 20, height = 15, units = "cm", dpi = 500)
