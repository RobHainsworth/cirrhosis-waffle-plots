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

##make spacer
squares_per_row<-25

xdf_nosurv[1,2]


data.frame(suboutcomes = c("HCC cases diagnosed",
                           "deaths from HCC",
                           "deaths from other causes",
                           "deaths from any cause",
                           "false alarms",
                           "intensified ultrasound follow-ups",
                           "CT/MRI scans",
                           "liver biopsies"),
           vals = c(squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[1,2]/squares_per_row)-
                                                         floor(xdf_nosurv[1,2]/squares_per_row)),
                    squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[2,2]/squares_per_row)-
                                                         floor(xdf_nosurv[2,2]/squares_per_row)),
                    squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[3,2]/squares_per_row)-
                                                         floor(xdf_nosurv[3,2]/squares_per_row)),
                    squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[4,2]/squares_per_row)-
                                                         floor(xdf_nosurv[4,2]/squares_per_row)),
                    squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[5,2]/squares_per_row)-
                                                         floor(xdf_nosurv[5,2]/squares_per_row)),
                    squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[6,2]/squares_per_row)-
                                                         floor(xdf_nosurv[6,2]/squares_per_row)),
                    squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[7,2]/squares_per_row)-
                                                         floor(xdf_nosurv[7,2]/squares_per_row)),
                    squares_per_row+
                      squares_per_row-squares_per_row*((xdf_nosurv[8,2]/squares_per_row)-
                                                         floor(xdf_nosurv[8,2]/squares_per_row))),
           outcomes=c("All diagnoses:",
                      "Benefits:",
                      "Benefits:",
                      "Benefits:",
                      "Harms:",
                      "Harms:",
                      "Harms:",
                      "Harms:"),
           int=rep("",8)
) -> xdf_spacer

xdf<-rbind(xdf,xdf_spacer)%>% 
  mutate(suboutcomes=paste(outcomes," ",suboutcomes))

xdf %>% 
  spread(key=int,value=vals)->xdf_wide

xdf_wide %>% 
  mutate(labels=paste(Surveillance,"with surveillance vs.",No_surveillance,"without"))->xdf_wide

xdf<- xdf_wide %>% 
  gather(key="int",value="vals",Surveillance,No_surveillance,V1,na.rm=F) 

###plot waffles

p<- xdf %>%
  mutate(int=factor(int,levels=c("No_surveillance","V1","Surveillance"))) %>% 
  count(labels, suboutcomes, int, wt = vals) %>%
  ggplot(
    aes(fill = int, values = n)
  ) +
  geom_waffle(
    n_rows = squares_per_row,
    size = 0.33, 
    colour = "white",
    flip = T,
    show.legend = F
  ) +
  geom_text(aes(x=1,y=25,label=labels),hjust=0)+
  scale_fill_manual(
    name = NULL,
    values = c("grey","white","black")
  ) +
  facet_wrap(~suboutcomes,
             nrow=2)+
  coord_equal() +
  theme_ipsum(grid="") +
  theme_enhance_waffle() +
  ##ggtitle(title1)+ 
  scale_colour_ipsum()+
  theme(
    strip.text = element_text(face = "bold", size = rel(1)))
p
