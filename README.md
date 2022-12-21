# Project-Bacteria
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)


#C10

aptatarun123 <-  read_csv("aptatac10rawpur.csv")
mutatedata_aptata_c10 <- aggregate(OD ~ Concentration + time + Treatment, aptatarun123, mean)


#######################

ggplot(mutatedata_aptata_c10, 
       aes(x = time, y = OD,
           # fill = as.factor(Con),
           colour = as.factor(Concentration) )) +
  geom_point() +
  # geom_smooth(formula = y~x+ I(x^2), method=NULL,
  geom_line(formula = y~x+ I(x^2), method=NULL,
            sd= FALSE, 
            show.legend = TRUE, span=0.7) +
  # annotate("text",x=48, y=0.05, hjust=0, label="y~x+ I(x^2)", size=5)+
  facet_grid(~Treatment) +
  theme(legend.position = "top")+
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.background = element_rect(color = "black"),
        strip.background = element_rect(fill = "lightgray"),
        strip.text.x = element_text(face="bold", size=12),
        legend.title=element_text(face="bold"),
        axis.text=element_text(size=12,colour = "black"),
        axis.title.y =element_text(size=12,face="bold", margin = margin(r=7)),
        axis.title.x =element_text(size=12,face="bold", margin = margin(t=7)),
        strip.text = element_text(size=12, face="bold", color="black"))+
  scale_color_manual(name = "RL concentration\n(ppm)",
                     values = c("0" = "black", "5" = "red", "10"= "purple",
                                "25"="blue", "50"= "darkgreen", "100"= "orange",
                                "250"= "skyblue", "500"="maroon", "750"="violet"),
                     aesthetics = "colour" ) +
  
  
  #   or for using with se = TRUE:
  #                     aesthetics = c("fill", "colour" ))+
  scale_y_continuous(limits =c(0, 1, by=0.1 ))+
  # scale_y_continuous(breaks = seq(0,1, by=0.1)) +
  scale_x_continuous(breaks= seq(0,96, by=24))+
  xlab("Time (hours)")+
  ylab("OD (595nm)")+
  labs(color= "Con\n(ppm)")

ggsave(filename="curve.aptataC10.png", height = 5, width = 9)

```
