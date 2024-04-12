install.packages("pacman")
library(pacman)
p_load(dplyr,magrittr,ggplot2,waffle,socviz,hrbrthemes,tidyr,forcats)

data.frame(suboutcomes = c("normal surveillance",
                           "false alarms with biopsies",
                           "false alarms without biopsies",
                           "survive regardless of surveillance",
                           "deaths averted",
                           "die despite surveillance",
                           "deaths from other causes"),
           vals = c(654.8,111.4,39.7,28.6,13.7,69.4,82.4),
           outcomes=c("normal surveillance",
                      "false alarm",
                      "false alarm",
                      "HCC diagnosis",
                      "HCC diagnosis",
                      "HCC diagnosis",
                      "deaths from other causes")
) -> xdf

##Largest remainder rounding 

xdf <- xdf %>% 
  mutate(
    remainder = vals - floor(vals),
    floored = floor(vals),
    rank=nrow(xdf)-rank(remainder)+1
  ) %>% 
  mutate(number = ifelse(1000 - sum(floored) >= rank, floored + 1, floored)) %>% 
  arrange(vals)

sum(xdf$number)

##Comparison between interventions
data.frame(suboutcomes = c("more cases diagnosed",
                           "fewer deaths from HCC",
                           "fewer deaths from other causes",
                           "fewer deaths from any cause",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "additional CT/MRI scans",
                           "liver biopsies"),
           vals = c(110,69,82,151,150,85,65,39),
           outcomes=c("Diagnosis of HCC",
                      "Benefits of surveillance",
                      "Benefits of surveillance",
                      "Benefits of surveillance",
                      "Harms from surveillance",
                      "Harms from surveillance",
                      "Harms from surveillance",
                      "Harms from surveillance"),
           int=rep("surv",8)
) -> xdf_surv

data.frame(suboutcomes = c("more cases diagnosed",
                           "fewer deaths from HCC",
                           "fewer deaths from other causes",
                           "fewer deaths from any cause",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "additional CT/MRI scans",
                           "liver biopsies"),
           vals = c(110,82,82,164,0,0,0,0),
           outcomes=c("Diagnosis of HCC",
                      "Benefits of surveillance",
                      "Benefits of surveillance",
                      "Benefits of surveillance",
                      "Harms from surveillance",
                      "Harms from surveillance",
                      "Harms from surveillance",
                      "Harms from surveillance"),
           int=rep("nosurv",8)
)->xdf_nosurv

xdf<-rbind(xdf_surv,xdf_nosurv)

xdf %>% 
  spread(key=int,value=vals)->xdf_wide

xdf_wide %>% 
  mutate(diff=abs(nosurv-surv)
        ## ,suboutcomes=paste(diff," ",suboutcomes)
         )->xdf_wide

newLabs <- c(paste(xdf_wide$diff,xdf_wide$suboutcomes,"(",xdf_wide$surv
                   ,"vs.",xdf_wide$nosurv
                   ,")"))
newLabs<-setNames(newLabs,nm=xdf_wide$suboutcomes)

xdf<- xdf_wide %>% 
  gather(key="int",value="vals",nosurv,diff,surv,na.rm=F) %>% 
  arrange(desc(int)) %>% 
  mutate(vals=abs(vals))

##subset data
xdf1<-filter(xdf, outcomes=="Diagnosis of HCC")
xdf2<-filter(xdf, outcomes=="Benefits of surveillance")
xdf3<-filter(xdf, outcomes=="Harms from surveillance")

title1<-c("Diagnosis of HCC")
title2<-c("Benefits of surveillance")
title3<-c("Harms of surveillance")

##plot waffles

p1<- xdf1 %>%
  mutate(int=factor(int,levels=c("surv","diff"))) %>% 
  count(suboutcomes, int, wt = vals) %>%
  ggplot(
    aes(fill = reorder(int,-int), values = n)
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
    values = c("#969696","dark green")
  ) +
  facet_wrap(~factor(suboutcomes,levels=c("more cases diagnosed")),
             nrow=1
             ,labeller=as_labeller(newLabs,default=label_wrap_gen(100)))+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle(title1)+ 
  scale_colour_ipsum()+ 
  theme(strip.text.x = element_text(size = 9))
p1
p2<- xdf2 %>%
  mutate(int=factor(int,levels=c("surv","diff"))) %>% 
  count(suboutcomes, int, wt = vals) %>%
  ggplot(
    aes(fill = reorder(int,-int), values = n)
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
    values = c("#969696","dark green")
  ) +
  facet_wrap(~factor(suboutcomes,levels=c("fewer deaths from any cause",
                                          "fewer deaths from HCC",
                                          "fewer deaths from other causes")),
             nrow=1
             ,labeller=as_labeller(newLabs,default=label_wrap_gen(100)))+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle(title2)+ 
  scale_colour_ipsum()+ 
  theme(strip.text.x = element_text(size = 9))
p2

p3<- xdf3 %>%
  mutate(int=factor(int,levels=c("nosurv","diff"))) %>% 
  count(suboutcomes, int, wt = vals) %>%
  ggplot(
    aes(fill = reorder(int,-int), values = n)
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
    values = c("dark red")
  ) +
  facet_wrap(~factor(suboutcomes,levels=c("false alarms",
                                          "intensified ultrasound follow-ups",
                                          "additional CT/MRI scans",
                                          "liver biopsies")),
             nrow=1
             ,labeller=as_labeller(newLabs,default=label_wrap_gen(100)))+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle(title3)+ 
  scale_colour_ipsum()+ 
  theme(strip.text.x = element_text(size = 9))
p3

##arrange waffles on page, adjusting ratio of width and height each waffle occupies

lay_out(list(p1,1:50,1:26),list(p2,1:50,27:100),list(p3,51:100,1:100))

