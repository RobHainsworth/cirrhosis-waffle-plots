install.packages("pacman")
library(pacman)
p_load(dplyr,magrittr,ggplot2,waffle,socviz,hrbrthemes,tidyr,forcats,layout,huxtable,cowplot)

##Comparison between interventions
data.frame(suboutcomes = c("HCC cases diagnosed",
                           "deaths from other causes",
                           "deaths with HCC",
                           "deaths from any cause",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "CT/MRI scans",
                           "liver biopsies"),
           vals = c(110,82,69,151,150,85,65,39),
           outcomes=c("All diagnoses:",
                      "Benefits:",
                      "Benefits:",
                      "Benefits:",
                      "Harms:",
                      "Harms:",
                      "Harms:",
                      "Harms:"),
           int=rep("Surveillance_vals",8)
) -> xdf_surv

data.frame(suboutcomes = c("HCC cases diagnosed",
                           "deaths from other causes",
                           "deaths with HCC",
                           "deaths from any cause",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "CT/MRI scans",
                           "liver biopsies"),
           vals = c(110,82,82,164,0,0,0,0),
           outcomes=c("All diagnoses:",
                      "Benefits:",
                      "Benefits:",
                      "Benefits:",
                      "Harms:",
                      "Harms:",
                      "Harms:",
                      "Harms:"),
           int=rep("No_surveillance_vals",8)
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
  arrange(int,desc(suboutcomes)) 

###plot waffles by outcome and arrange

##benefits

xdf %>% 
  filter(outcomes=="Benefits:",var=="ce_vals",substr(suboutcomes,1,13)!="deaths from a") %>% 
  spread(key=int,value=vals) %>% 
  arrange(factor(suboutcomes,levels=c("deaths from other causes",
                                      "deaths from other causes  spacer",
                                      "deaths with HCC",
                                      "deaths with HCC  spacer",
                                      "deaths from any cause",
                                      "deaths from any cause  spacer"))) %>% 
  mutate("No surveillance"=ceiling(cumsum(No_surveillance_vals)/squares_per_row),
         Surveillance=ceiling(cumsum(Surveillance_vals)/squares_per_row),
         surv_outcome_total=sum(Surveillance_vals),
         no_surv_outcome_total=sum(No_surveillance_vals),
         rank=row_number())->xdf_wide2

xdf2<- xdf_wide2 %>% 
  gather(key="int",value="height",
         Surveillance,
         "No surveillance",
         na.rm=F) %>% 
  mutate(vals=ifelse(int=="Surveillance",Surveillance_vals
                     ,No_surveillance_vals),labels=ifelse(var==" spacer","",paste(vals,suboutcomes)),
         outcome_total=ifelse(int=="Surveillance",surv_outcome_total,no_surv_outcome_total),
    suboutcomes_new=paste(vals,suboutcomes))
               
p1<- xdf2 %>%
  mutate(int=factor(int,levels=c("No surveillance","Surveillance")),
         suboutcomes=factor(suboutcomes,levels=c("deaths from other causes",
                                                 ##"deaths from other causes  spacer",
                                                 "deaths with HCC"
                                                 # ,
                                                 # "deaths with HCC  spacer"
                                                 ))) %>% 
  count(int,suboutcomes, wt = vals, height,labels,outcome_total,rank,suboutcomes_new) %>%
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
  geom_label(colour="white",aes(x=1,y=floor(outcome_total/squares_per_row)+0.5+2*rank,
                 hjust=0,
                 label=suboutcomes_new
                              ,size=5))+
  scale_fill_manual(
  name = NULL
  ,
  values = c("dark blue",
             ##"white",
             "purple"
             #, "white"
             )
  ) +
  facet_wrap(~int,
             nrow=1)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle("Comparison of benefits")+ 
  ##scale_colour_ipsum()+
  theme(legend.position="none")+
  theme(
    strip.text = element_text(margin(b=2),face = "bold", size = rel(1.25)))
p1

#all diagnoses

xdf %>% 
  filter(outcomes=="All diagnoses:") %>% 
  spread(key=int,value=vals) %>% 
  mutate("No surveillance"=ceiling(cumsum(No_surveillance_vals)/squares_per_row),
         Surveillance=ceiling(cumsum(Surveillance_vals)/squares_per_row))->xdf_wide2

xdf2<- xdf_wide2 %>% 
  gather(key="int",value="height",
         Surveillance,
         "No surveillance",
         na.rm=F) %>% 
  mutate(vals=ifelse(int=="Surveillance",Surveillance_vals
                     ,No_surveillance_vals
  ),
  labels=ifelse(var==" spacer","",
                ifelse(int=="Surveillance",
                       ifelse(suboutcomes=="deaths with HCC",
                              paste("comprising:",vals,suboutcomes),
                              paste(vals,suboutcomes
                                    ##,"with surveillance"
                              )),
                       paste(vals,suboutcomes
                             ##,"without surveillance"
                       ))))

p2<- xdf2 %>%
  mutate(int=factor(int,levels=c("No surveillance","Surveillance")),
         suboutcomes=factor(suboutcomes,levels=c("HCC cases diagnosed",
                                                 "HCC cases diagnosed  spacer"))) %>% 
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
  geom_label(colour="white",aes(x=1,y=height+1.5,hjust=0,label=labels,size=5))+
  scale_fill_manual(
    name = NULL,
    values = c("black",
               "white")
  ) +
  facet_wrap(~int,
             nrow=1)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle("Comparison of diagnoses")+ 
  scale_colour_ipsum()+
  theme(legend.position="none")+
  theme(
    strip.text = element_text(vjust=1,face = "bold", size = rel(1.25)))
p2

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

p3<- xdf2 %>%
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
  geom_label(colour="white",aes(x=1,y=height+1.5,hjust=0,
                                label=factor(labels,levels=c(
                                  "39 liver biopsies",
                                  "85 intensified ultrasound follow-ups",
                                  "150 false alarms",
                                  "65 CT/MRI scans"
                                )),
                                size=5))+
  scale_fill_manual(
    name = NULL,
    values = c("black",
               "white",
               "black",
               "white",
               "black",
               "white",
               "black",
               "white")
  ) +
  facet_wrap(~int,
             nrow=2)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ggtitle("Harms of surveillance")+ 
  scale_colour_ipsum()+
  theme(legend.position="none")+
  theme(
    strip.text = element_blank()
    ## element_text(face = "bold", size = rel(1.5))
  )
p3

right_col<-plot_grid(p2,p1,ncol=1,rel_heights = c(37,63))
plot_grid(right_col,p3,rel_widths=c(65,35))
