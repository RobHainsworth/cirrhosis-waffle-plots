install.packages("pacman")
library(pacman)
p_load(dplyr,magrittr,ggplot2,waffle,socviz,hrbrthemes,tidyr,forcats,layout,huxtable)

##Comparison between interventions
data.frame(suboutcomes = c("HCC cases diagnosed",
                           "deaths from HCC",
                           "deaths from other causes",
                           "deaths from any cause",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "CT/MRI scans",
                           "liver biopsies"),
           vals = c(110,69,82,151,150,85,65,39),
           outcomes=c("All diagnoses:",
                      "Benefits:",
                      "Benefits:",
                      "Benefits:",
                      "Harms:",
                      "Harms:",
                      "Harms:",
                      "Harms:"),
           int=rep("Surveillance",8)
) -> xdf_surv

data.frame(suboutcomes = c("HCC cases diagnosed",
                           "deaths from HCC",
                           "deaths from other causes",
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
           int=rep("No_surveillance",8)
)->xdf_nosurv

xdf<-rbind(xdf_surv,xdf_nosurv) 


squares_per_row<-25

xdf %>% 
  spread(key=int,value=vals)->xdf_wide

xdf_wide %>% 
  mutate(labels=paste(Surveillance,suboutcomes,"with surveillance vs.",No_surveillance,"without"),
         "No_surveillance spacer"=round(squares_per_row+
           squares_per_row*2-squares_per_row*((No_surveillance/squares_per_row)-
                                              floor(No_surveillance/squares_per_row))),
         "Surveillance spacer"=round(squares_per_row+
              squares_per_row*2-squares_per_row*((Surveillance/squares_per_row)-
                                              floor(Surveillance/squares_per_row))))->xdf_wide

xdf<- xdf_wide %>% 
  gather(key="int",value="vals",
         Surveillance,No_surveillance,"No_surveillance spacer","Surveillance spacer",
         na.rm=F) %>% 
  mutate(var=substr(int,nchar(int)-6,nchar(int)),
         suboutcomes=ifelse(var==" spacer",paste(suboutcomes," spacer"),suboutcomes),
         int=sapply(strsplit(int," "), "[[", 1)
         ) %>% 
  arrange(int,suboutcomes) 

xdf %>% 
  spread(key=int,value=vals) %>% 
  mutate(No_surveillance_height=ceiling(cumsum(No_surveillance)/squares_per_row),
         Surveillance_height=ceiling(cumsum(Surveillance)/squares_per_row))->xdf_wide2

xdf2<- xdf_wide2 %>% 
  gather(key="int",value="height",
         Surveillance_height,No_surveillance_height,
         na.rm=F) %>% 
  mutate(vals=ifelse(int=="Surveillance_height",Surveillance,No_surveillance),
         labels=ifelse(var==" spacer","",
                       ifelse(int=="Surveillance_height",
                              paste(vals,suboutcomes,"with surveillance"),
                              paste(vals,suboutcomes,"without surveillance"))))

         
###plot waffles



p<- xdf2 %>%
  mutate(int=factor(int,levels=c("No_surveillance_height","Surveillance_height")),
         suboutcomes=factor(suboutcomes,levels=c("CT/MRI scans",
                                                 "CT/MRI scans  spacer",
                                                 "deaths from any cause",
                                                 "deaths from any cause  spacer",
                                                 "deaths from HCC",
                                                 "deaths from HCC  spacer",
                                                 "deaths from other causes",
                                                 "deaths from other causes  spacer",
                                                 "false alarms",
                                                 "false alarms  spacer",
                                                 "HCC cases diagnosed",
                                                 "HCC cases diagnosed  spacer",
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
