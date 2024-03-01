library(pacman)
p_load(here,
       tidyverse,
       magrittr,
       waffle,
       hrbrthemes,
       glue,
       forcats)

fnPad <- function(n, w = 25, l = 4) {(trunc((n-1) / w)+1) * w - n + w*l}

fnHCC_Waffle <- function(lstIn, yrs, w) {
  
  tblX <- tribble(
    ~col, ~grp,  ~nn,        ~cat,                                                          ~label,   ~face,          ~fll,   ~ht,
    1L,   2L,  143,    "DieOth",       "{nnn} will die from other causes\nduring the {yrs} years",  "bold",       "black",  2.5,
    1L,   2L,  -6L,     "head2",                                                              NA, "plain",       "white",    NA,
    1L,   1L,  143,      "Norm",                                                              NA, "plain",        "blue",    NA,
    1L,   1L,  -1L,     "head1",       "{nnn} will have normal surveillance\nfor all {yrs} years",  "bold",       "white",  1.5,
    2L,   3L,  143, "HCC.avert",                                           "{nn} deaths averted", "plain",        "cyan",   1.5,
    2L,   3L,  -3L,     "spac2",                                                              NA, "plain",       "white",    NA,
    2L,   3L,  143,  "HCC.surv",                       "{nn} survive regardless of surveillance", "plain", "forestgreen",   1.5,
    2L,   3L,  -3L,     "spac3",                                                              NA, "plain",       "white",    NA,
    2L,   3L,  143,   "HCC.die",                                 "{nn} die despite surveillance", "plain",       "black",   1.5,
    2L,   3L,  -2L,     "head3",  "{nnn} will receive a diagnosis of HCC\nduring the {yrs} years",  "bold",       "white",  2.5,
    2L,   3L, -600,     "spac4",                                                              NA, "plain",       "white",    NA,
    2L,   4L,  143,   "FA.noBx",                            "{nn} false alarms without biopsies", "plain",        "pink",   1.5,
    2L,   4L,  -3L,     "spac6",                                                              NA, "plain",       "white",    NA,
    2L,   4L,  143,     "FA.Bx",                               "{nn} false alarms with biopsies", "plain",         "red",   1.5,
    2L,   4L,  -2L,     "head4", "{nnn} will have at least 1 false alarm\nduring the {yrs} years",  "bold",       "white",  2.5
  )
  
  tblX %<>% 
    rows_update(lstIn %>%
                  as_tibble() %>%
                  pivot_longer(everything(),
                               names_to  = "cat",
                               values_to = "nn"),
                by = "cat") %>%
    group_by(grp) %>%
    mutate(nnn = cumsum(ifelse(nn>0, nn, 0))) %>% 
    ungroup() %>% 
    mutate(nn = ifelse(nn<0, ifelse(nn<=-100, -nn/100*w, fnPad(n = lag(nn), w = w, l = -nn)), nn)) %>% 
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
    scale_y_continuous(expand = expansion(add = c(0, 3))) +
    theme(strip.text = element_blank(),
          panel.spacing = unit(1, "cm"),
          plot.margin = margin())
}

fnHCC_Waffle(lstIn = list(Norm      = 654,
                          FA.noBx   = 111,
                          FA.Bx     = 39,
                          DieOth    = 82,
                          HCC.surv  = 28,
                          HCC.avert = 13,
                          HCC.die   = 69),
             yrs = 5,
             w   = 25)

ggsave(here("waffle.jpg"), width = 20, height = 15, units = "cm", dpi = 500)
