install.packages("pacman")
library(pacman)
p_load(readxl,magrittr,dplyr,tidyverse,waffle,hrbrthemes,cowplot)

wb=read_excel("C:\\Users\\mbmhtrh2\\OneDrive - The University of Manchester\\Cirrhosis\\LY_data.xlsx")

tb<- wb %>%
  summarise(across(`NoSurv_Alive_no_diag_half-cycle`:`Surv_dead_half-cycle`,sum))

tb_long<- add_rownames(tb) %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value)

data.frame(suboutcomes = c("years lived without diagnosis",
                           "years lived with diagnosis",
                           "years lived overall",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "CT/MRI scans",
                           "liver biopsies"),
           vals = c(round(tb$`Surv_alive_no_diag_half-cycle`),
                    round(tb$`Surv_alive_diag_half-cycle`),
                    round(tb$`Surv_alive_no_diag_half-cycle`+tb$`Surv_alive_diag_half-cycle`),
                    150,85,65,39),
           outcomes=c("years lived",
                      "years lived",
                      "years lived",
                      "Harms:",
                      "Harms:",
                      "Harms:",
                      "Harms:"),
           int=rep("Surveillance_vals",7)
) -> xdf_surv

data.frame(suboutcomes = c("years lived without diagnosis",
                           "years lived with diagnosis",
                           "years lived overall",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "CT/MRI scans",
                           "liver biopsies"),
           vals = c(round(tb$`NoSurv_Alive_no_diag_half-cycle`),
                    round(tb$`NoSurv_live_diag_half-cycle`),
                    round(tb$`NoSurv_Alive_no_diag_half-cycle`+tb$`NoSurv_live_diag_half-cycle`),
                    0,0,0,0),
           outcomes=c("years lived",
                      "years lived",
                      "years lived",
                      "Harms:",
                      "Harms:",
                      "Harms:",
                      "Harms:"),
           int=rep("No_surveillance_vals",7)
)->xdf_nosurv

xdf<-rbind(xdf_surv,xdf_nosurv) 


squares_per_row<-25

xdf %>% 
  spread(key=int,value=vals)->xdf_wide

xdf_wide<- xdf_wide %>% 
  mutate(no_surv_height=floor(No_surveillance_vals/squares_per_row), 
         surv_height=floor(Surveillance_vals/squares_per_row),
         cond=abs(no_surv_height-surv_height))

xdf_wide %>% 
  mutate(labels=paste(Surveillance_vals,suboutcomes,"with surveillance vs.",No_surveillance_vals,"without"),
         "No_surveillance_vals spacer"=round(ifelse(surv_height>no_surv_height,squares_per_row*cond,0)+
                                               squares_per_row*2+squares_per_row*(ceiling(No_surveillance_vals/squares_per_row)-
                                                                                    (No_surveillance_vals/squares_per_row))),
         "Surveillance_vals spacer"=round(ifelse(no_surv_height>surv_height,squares_per_row*cond,0)+
                                            squares_per_row*2+squares_per_row*(ceiling(Surveillance_vals/squares_per_row)-
                                                                                 (Surveillance_vals/squares_per_row))))->xdf_wide

xdf<- xdf_wide %>% 
  gather(key="int",value="vals",
         Surveillance_vals,No_surveillance_vals,"No_surveillance_vals spacer","Surveillance_vals spacer",
         na.rm=F) %>% 
  mutate(var=substr(int,nchar(int)-6,nchar(int)),
         suboutcomes=ifelse(var==" spacer",paste(suboutcomes," spacer"),suboutcomes),
         int=sapply(strsplit(int," "), "[[", 1)
  ) %>% 
  arrange(int,suboutcomes) 

xdf %>% 
  spread(key=int,value=vals) %>% 
  mutate("No surveillance"=ceiling(cumsum(No_surveillance_vals)/squares_per_row),
         Surveillance=ceiling(cumsum(Surveillance_vals)/squares_per_row))->xdf_wide2

xdf2<- xdf_wide2 %>% 
  gather(key="int",value="height",
         Surveillance,"No surveillance",
         na.rm=F) %>% 
  mutate(vals=ifelse(int=="Surveillance",Surveillance_vals,No_surveillance_vals),
         labels=ifelse(var==" spacer","",
                       ifelse(int=="Surveillance",
                              paste(vals,suboutcomes,"with surveillance"),
                              paste(vals,suboutcomes,"without surveillance"))))


###plot waffles- all together

p<- xdf2 %>%
  mutate(int=factor(int,levels=c("No surveillance","Surveillance")),
         suboutcomes=factor(suboutcomes,levels=c("CT/MRI scans",
                                                 "CT/MRI scans  spacer",
                                                 "false alarms",
                                                 "false alarms  spacer",
                                                 "intensified ultrasound follow-ups",
                                                 "intensified ultrasound follow-ups  spacer",
                                                 "liver biopsies",
                                                 "liver biopsies  spacer",
                                                 "years lived overall",
                                                 "years lived overall  spacer",
                                                 "years lived with diagnosis",
                                                 "years lived with diagnosis  spacer",
                                                 "years lived without diagnosis",
                                                 "years lived without diagnosis  spacer"))) %>% 
  count(int,suboutcomes, wt = vals, height,labels) %>%
  ggplot(
    aes(fill = suboutcomes, values = n)
  ) +
  geom_waffle(
    n_rows = squares_per_row,
    size = 0.33, 
    colour = "white",
    flip = T,
    show.legend = F
  ) +
  geom_text(aes(x=1,y=height+1.5,hjust=0,label=labels),size=2.5)+
  scale_fill_manual(
    name = NULL,
    values = c("black",
               "white","black",
               "white","black",
               "white","black",
               "white","black",
               "white","black",
               "white","black",
               "white")
  ) +
  facet_wrap(~int,
             nrow=1)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ##ggtitle(title1)+ 
  scale_colour_ipsum()+
  theme(
    strip.text = element_text(face = "bold", size = rel(0.75)))
p

###plot waffles by outcome and arrange

##years lived

xdf %>% 
  filter(outcomes=="years lived") %>% 
  spread(key=int,value=vals) %>% 
  mutate("No surveillance"=ceiling(cumsum(No_surveillance_vals)/squares_per_row),
         Surveillance=ceiling(cumsum(Surveillance_vals)/squares_per_row))->xdf_wide2

xdf2<- xdf_wide2 %>% 
  gather(key="int",value="height",
         Surveillance,"No surveillance",
         na.rm=F) %>% 
  mutate(vals=ifelse(int=="Surveillance",Surveillance_vals,No_surveillance_vals),
         labels=ifelse(var==" spacer","",
                       ifelse(int=="Surveillance",
                              paste(vals,suboutcomes
                                    ##,"with surveillance"
                              ),
                              paste(vals,suboutcomes
                                    ##,"without surveillance"
                              ))))
p1<- xdf2 %>%
  mutate(int=factor(int,levels=c("No surveillance","Surveillance")),
         suboutcomes=factor(suboutcomes,levels=c("years lived overall",
                                                 "years lived overall  spacer",
                                                 "years lived with diagnosis",
                                                 "years lived with diagnosis  spacer",
                                                 "years lived without diagnosis",
                                                 "years lived without diagnosis  spacer"))) %>% 
  count(int,suboutcomes, wt = vals, height,labels) %>%
  ggplot(
    aes(fill = suboutcomes, values = n)
  ) +
  geom_waffle(
    n_rows = squares_per_row,
    size = 0.33, 
    colour = "white",
    flip = T,
    show.legend = F
  ) +
  geom_text(aes(x=1,y=height+1.5,hjust=0,label=labels),size=5)+
  scale_fill_manual(
    name = NULL,
    values = c("black",
               "white",
               "black",
               "white",
               "black",
               "white")
  ) +
  facet_wrap(~int,
             nrow=1)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle("Comparison of life expectancy")+ 
  scale_colour_ipsum()+
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5)))
p1


#Harms

xdf %>% 
  filter(outcomes=="Harms:",int=="Surveillance_vals") %>% 
  spread(key=int,value=vals) %>% 
  mutate(
    ##"No surveillance"=ceiling(cumsum(No_surveillance_vals)/squares_per_row),
    Surveillance=ceiling(cumsum(Surveillance_vals)/squares_per_row))->xdf_wide2

xdf2<- xdf_wide2 %>% 
  gather(key="int",value="height",
         Surveillance,
         ##"No surveillance",
         na.rm=F) %>% 
  mutate(vals=ifelse(int=="Surveillance",Surveillance_vals
                     ## ,No_surveillance_vals
  ),
  labels=ifelse(var==" spacer","",
                ifelse(int=="Surveillance",
                       paste(vals,suboutcomes
                             ##,"with surveillance"
                       ),
                       paste(vals,suboutcomes
                             ##,"without surveillance"
                       ))))

p2<- xdf2 %>%
  mutate(int=factor(int,levels=c("No surveillance","Surveillance")),
         suboutcomes=factor(suboutcomes,levels=c("CT/MRI scans",
                                                 "CT/MRI scans  spacer",
                                                 "false alarms",
                                                 "false alarms  spacer",
                                                 "intensified ultrasound follow-ups",
                                                 "intensified ultrasound follow-ups  spacer",
                                                 "liver biopsies",
                                                 "liver biopsies  spacer"))) %>% 
  count(int,suboutcomes, wt = vals, height,labels) %>%
  ggplot(
    aes(fill = suboutcomes, values = n)
  ) +
  geom_waffle(
    n_rows = squares_per_row,
    size = 0.33, 
    colour = "white",
    flip = T,
    show.legend = F
  ) +
  geom_text(aes(x=1,y=height+1.5,hjust=0,label=labels),size=5)+
  scale_fill_manual(
    name = NULL,
    values = c("dark red",
               "white",
               "dark red",
               "white",
               "dark red",
               "white",
               "dark red",
               "white")
  ) +
  facet_wrap(~int,
             nrow=2)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle("Harms of surveillance (amongst 1000 people)")+ 
  scale_colour_ipsum()+
  theme(
    strip.text = element_blank()
    ## element_text(face = "bold", size = rel(1.5))
  )
p2

plot_grid(p2,p1,ncol=1,rel_widths = c(34,66))

##comparison approach

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
data.frame(suboutcomes = c("additional cases diagnosed",
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

data.frame(suboutcomes = c("additional cases diagnosed",
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
  mutate(diff=abs(nosurv-surv),
         suboutcomes=paste(diff," ",suboutcomes))->xdf_wide

xdf<- xdf_wide %>% 
  gather(key="int",value="vals",nosurv,diff,na.rm=F) %>% 
  select(-c(surv)) %>% 
  arrange(desc(int)) %>% 
  mutate(vals=abs(vals))

##subset data
xdf1<-filter(xdf, outcomes=="Diagnosis of HCC")
xdf2<-filter(xdf, outcomes=="Benefits of surveillance")
xdf3<-filter(xdf, outcomes=="Harms from surveillance")

title1<-c("Diagnosis of HCC")
title2<-c("Benefits of surveillance (amongst 1000 people)")
title3<-c("Harms of surveillance")

##plot waffles

p3<- xdf1 %>%
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
    values = c("#969696","dark green")
  ) +
  facet_wrap(~suboutcomes,
             nrow=1)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle(title1)+ 
  scale_colour_ipsum()
p3

p4<- xdf2 %>%
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
    values = c("#969696","dark green")
  ) +
  facet_wrap(~suboutcomes,
             nrow=1)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle(title2)+ 
  scale_colour_ipsum()
p4

          
top_row<-plot_grid(p2,p4,ncol=2,rel_widths = c(33,67))
bottom_row<-plot_grid(p3,p1,ncol=2,rel_widths = c(25,75))
plot_grid(top_row,p1,nrow=2,rel_heights = c(64,36))
