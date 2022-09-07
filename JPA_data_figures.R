#load libraries
library(tidyverse)
library(ggthemes)
library(rstatix)
library(patchwork)

#General note: 
#this R script includes all data necessary to produce figures from paper: 

#Smith et al., 2020: When Lithics Hit Bones: Evaluating the Potential
# of a Multifaceted Experimental Protocol to Illuminate
# Middle Palaeolithic Weapon Technology. Journal of Paleolithic Archaeology.

#Figure 3: Velocity and kinetic energy
#Loads dataset for Boar dataset with velocity and Kinetic energy
Boar_dataset <- read_csv("Data/Boar_dataset.csv")

#Boxplot velocity for Boar dataset throw and thrust avg. velocity
Boar_vel_plot <- 
  ggplot(Boar_dataset)+
  aes(x=Technology)+
  aes(y=Target.v..m.s.)+
  geom_boxplot(alpha=.25, width = .5)+
  aes(colour = Technology, fill = Technology)+
  scale_fill_colorblind()+
  scale_colour_colorblind()+
  geom_point(alpha=.25)+
  theme_bw()+
  labs(x="Delivery method")+
  labs(y="Velocity at target (m/s)")+
  labs(fill = "Delivery")+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  scale_y_continuous (breaks = seq(0,30,5))+
  theme(legend.position = "none")


#Boxplot for Boar dataset throw and thrust experiment kinetic energy.
Boar_KE_plot <- 
  ggplot(Boar_dataset)+
  aes(x= Technology)+
  aes(y= Ekin..J.)+
  geom_boxplot(alpha=.25, width = .5)+
  aes(colour = Technology, fill = Technology)+
  scale_fill_colorblind()+
  scale_colour_colorblind()+
  geom_point(alpha=.25)+
  theme_bw()+
  labs(x="Delivery method")+
  labs(y="Kinetic energy (J)")+
  labs(fill = "Delivery")+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  scale_y_continuous (breaks = seq(0,120,20))+
  theme(legend.position = "none")


#Iovita et al experimental dataset
Iovita_exp_dataset <- read_csv("Data/Iovita_exp_dataset.csv")

#Boxplot for experimental dataset throw and thrust avg. velocity
Iovita_vel_plot <- 
  ggplot(Iovita_exp_dataset)+
  aes(x=Delivery)+
  aes(y= m.s)+
  geom_boxplot(alpha=.25, width = .5)+
  aes(colour = Delivery, fill = Delivery)+
  scale_fill_colorblind()+
  scale_colour_colorblind()+
  geom_point(alpha=.25)+
  theme_bw()+
  labs(x="Delivery method")+
  labs(y="Velocity at target (m/s)")+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  scale_y_continuous (breaks = seq(0,40,5))+
  theme(legend.position = "none")

#Boxplot for experimental dataset throw and thrust kinetic energy
Iovita_KE <- 
  ggplot(Iovita_exp_dataset)+
  aes(x=Delivery)+
  aes(y= E.J)+
  geom_boxplot(alpha=.25, width = .5)+
  aes(colour = Delivery, fill = Delivery)+
  scale_fill_colorblind()+
  scale_colour_colorblind()+
  geom_point(alpha=.25)+
  theme_bw()+
  labs(x="Delivery method")+
  labs(y="Kinetic energy (J)")+
  stat_boxplot(geom ='errorbar', width = 0.4)+
  scale_y_continuous (breaks = seq(0,120,20))+
  theme(legend.position = "none")


#Use patchwork to create a combined figure (Figure 3 in manuscript)
vel_KE_combined <- 
Boar_vel_plot+Iovita_vel_plot+Boar_KE_plot+Iovita_KE+
  plot_annotation(tag_levels = 'A',
                  caption = 
                    "Fig. 3 Boxplots of velocity at target (m/s) (left) and kinetic energy (J) (right) for each delivery method during both the replicative (A and B) and controlled experiments (C and D); black line indicates median")

#save to figure_output folder
ggsave("figure_output/vel_KE_combined.tiff",vel_KE_combined, dpi = 300,width =29.7, height = 21, units = "cm")


#Figure 4: Plot of C/C2 ratios vs. velocity in Boar experiments
crack.front <- read.csv("Data/c.c2.csv",header = T, stringsAsFactors = F)

#lithic crack front plot
cf_plot <- 
    ggplot(crack.front)+
    aes(x=m.s)+
    aes(y=c.c2)+
    geom_point(aes(shape = Material, color = Delivery))+
    scale_colour_colorblind()+ ###selects colours good for colourblind persons
    scale_x_continuous(limits = c(0,30), expand = c(0,0))+
    scale_y_continuous(limits = c(0,0.5), expand = c(0,0))+
    geom_smooth(method = "lm", se=F)+
    theme_bw()+
    xlab("velocity (m/s)")+
    ylab("C/C2")+
    labs(caption = 
    "Fig. 4 Plot of C/C2 ratios against velocity at target entry 
    with different shapes illustrating different delivery 
    methods; the lines illustrate defined loading rate ranges, 
    redrawn after Schlösser 2015, Tab19; the loading rate ranges 
    (Quasi-static, Rapid and Dynamic) are taken from 
    calculations from (Hutchings 2011)")+
    geom_hline(yintercept=0.10, linetype="dashed")+
    geom_hline(yintercept=0.38, linetype="dashed")+
    annotate("text", x = 10, y = 0.45, label = "Dynamic")+
    annotate("text", x = 10, y = 0.2, label = "Rapid")+
    annotate("text", x = 10, y = 0.05, label = "Quasi-static")

#save to figure_output folder
ggsave("figure_output/cf_plot.tiff",cf_plot, dpi = 300,width =29.7, height = 21, units = "cm")


#Figure 5: PIMS recorded during Boar experiments by delivery method and KE

#load in dataset for figure
  PIM_damage_boar_1 <- read_csv("Data/PIM_damage_Boar_1.csv")


##Plot of PIMs from Boar by KE
Boar_PIM_Type <- 
  ggplot(PIM_damage_boar_1)+
  aes(x=Technology)+
  labs(x= "Delivery method")+
  aes(y=Kinetic.energy..J.)+
  labs(y="Kinetic energy (J)")+
  aes(shape=Type, color = Type)+
  geom_point(position = position_dodge(width = 0.25))+
  theme_bw(base_size = 10)+
  scale_y_continuous (limits = c(0,70),breaks = seq(0,70,10))+
  scale_colour_colorblind()+
  labs(caption = "Fig. 5 PIMs recorded throughout replicative experiments plotted by delivery method and kinetic energy
       (annotated numbers are individual shot numbers)")+
  geom_text(aes(label=Shot.no.),check_overlap = T, color = "black", nudge_x = 0.25, size = 2)+
  theme(plot.caption = element_text(hjust = 0))

#save to figure_output folder
ggsave("figure_output/Boar_PIM_Type.tiff",Boar_PIM_Type, dpi = 300,width =29.7, height = 21, units = "cm")


#Figure 8: PIM damage comparing Boar and lab experiments and impact angle

#load in dataset for Boar
PIM_damage_boar_2 <- read_csv("Data/PIM_damage_Boar_2.csv")

#Plot PIMs on Boar for throwing and thrusting
PIM_KE_Boar <- 
  ggplot(PIM_damage_boar_2)+
  aes(x=Technology)+
  labs(x= "Delivery method")+
  aes(y=Kinetic.energy..J.)+
  labs(y="Kinetic energy (J)")+
  geom_point(position = position_dodge(width = 0.25))+
  aes(shape = Type, color=Technology)+
  theme_bw(base_size=10)+
  scale_y_continuous (limits = c(0,70),breaks = seq(0,70,10))+
  scale_colour_colorblind()+
  scale_shape_manual(values = c(0,1,2))+
  labs(shape = "Type")+
  geom_text(aes(label=Shot.no.),hjust = 0.25, vjust = 0.25, 
            nudge_x = 0.25, size =2, check_overlap =  TRUE, color = "black")+
  theme(plot.caption = element_text(hjust = 0))

#load in dataset for lab replicative experiments
PIMS_real_impact_angle <- read_csv("Data/PIMS_lab_impact_angle.csv")

#Plot PIMs lab replicative experiment for Delivery, type and energy
PIMS_Real_impact_angle_delivery <- 
  ggplot(PIMS_real_impact_angle)+
  aes(x=position)+
  labs(x="Angle of Impact")+
  aes(y=E.J)+
  labs(y="Kinetic energy (J)")+
  scale_x_discrete(limits=c("90","75","60","45","30"))+
  scale_color_colorblind()+
  scale_shape_manual(values = c(0,1,2))+
  aes(shape=Type,color=Delivery)+
  geom_point(position = position_dodge(width = 0.50))+
  theme_bw(base_size = 8)

#make combined figure (Figure 8)
PIM_Boar_Real_combined <- 
PIM_KE_Boar/PIMS_Real_impact_angle_delivery+
  plot_annotation('a',caption = 
"Fig. 8 Comparison of PIMs recorded on a replicative wild pig experiments (this paper) and b 
controlled experiment bone plates (Iovita et al. 2014) plotted against kinetic energy (J), delivery method 
and impact angle [only for lab-based experiments]; square: drag; circle: fracture; triangle: pit. 
Coloured by delivery method.Note this does not include the total number of PIMs for both types of 
experiment but incorporates comparable PIM types. Shot numbers included for replicative experiments (a)")

PIM_Boar_Real_combined

#save to figure_output folder
ggsave("figure_output/PIM_Boar_Real_combined.tiff",PIM_Boar_Real_combined, dpi = 300,width =29.7, height = 21, units = "cm")


