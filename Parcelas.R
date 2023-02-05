

rm(list = ls()) # Eliminar objetos del ambient de trabajo
cat("\014") # Eliminar historial de la consola

### ----- Cargar librerias -----

pacman::p_load("dplyr", "ggplot2", "tidyverse", "readxl", "nlme", "openxlsx",
               "tidyr", "lmfor","broom.mixed", "zoo", "lmtest", "modelsummary",
               "cowplot", "gridExtra", "caTools", "ggpubr","ggpmisc",
               "pastecs", "broom", "broom.mixed", "purrr",
               "performance", install = F)

### ----- Importar de excel -----

ejemplo <- 
  read_excel("C:/Users/Nava/Desktop/Doctorado COLPOS/Herramientas silvicolas/Parcelas ejemplo.xlsx", 
                 sheet = "resumen", range = "A1:G89") 

# ========================================================================================
plot1 <- ggplot(data=ejemplo)+
  geom_line(size=1,aes(x=edad,y=HD,  linetype=as.factor(tipo), 
                         color=as.factor(parcela)))+
  geom_point(aes(x=edad,y=HD, shape=as.factor(parcela), color=as.factor(parcela)))+
  labs(fill="",color="Parcela",shape="Parcela", linetype= "Dato")+
  geom_vline(xintercept = 40, linetype="dashed", color = "red", size=1)+
  theme_bw()+ labs(x="Edad (años)", y = "Altura dominante (m)")+
  #scale_x_continuous(breaks = seq(5,25, by=5))+
  #scale_y_continuous(limits = c(-5,5))+
  theme(axis.text = element_text(size = 12, color = "black"),
        legend.position = c(.1, 0.90), 
        legend.text = element_text(color = "black", size=8, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(),  axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(0,0,0,0, unit="cm"));plot1

ggsave("C:/Users/Nava/Desktop/Doctorado COLPOS/Herramientas silvicolas/Plot1.tiff", 
       dpi=300, height=15, width=20, units="cm")
# ========================================================================================

plot2 <- ggplot(data=ejemplo)+
  geom_line(size=1,aes(x=edad,y=Abp,  linetype=as.factor(tipo), 
                       color=as.factor(parcela)))+
  geom_point(aes(x=edad,y=Abp, shape=as.factor(parcela), color=as.factor(parcela)))+
  labs(fill="",color="Parcela",shape="Parcela", linetype= "Dato")+
  geom_vline(xintercept = 40, linetype="dashed", color = "red", size=1)+
  theme_bw()+ labs(x="Edad (años)", y = expression("Área basal"~(m^2~ha^-1)))+
  #scale_x_continuous(breaks = seq(5,25, by=5))+
  #scale_y_continuous(limits = c(-5,5))+
  theme(axis.text = element_text(size = 12, color = "black"),
        legend.position = c(.15, 0.85), 
        legend.text = element_text(color = "black", size=8, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(0,0,0,0, unit="cm"));plot2


# ========================================================================================

plot3 <- ggplot(data=ejemplo)+
  geom_line(size=1,aes(x=edad,y=Vp,  linetype=as.factor(tipo), 
                       color=as.factor(parcela)))+
  geom_point(aes(x=edad,y=Vp, shape=as.factor(parcela), color=as.factor(parcela)))+
  labs(fill="",color="Parcela",shape="Parcela", linetype= "Dato")+
  geom_vline(xintercept = 40, linetype="dashed", color = "red", size=1)+
  theme_bw()+ labs(x="Edad (años)", y = expression("Volumen"~(m^3~ha^-1)))+
  #scale_x_continuous(breaks = seq(5,25, by=5))+
  #scale_y_continuous(limits = c(-5,5))+
  theme(axis.text = element_text(size = 12, color = "black"),
        legend.position = c(.15, 0.85), 
        legend.text = element_text(color = "black", size=8, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(0,0,0,0, unit="cm"));plot3


# ========================================================================================

plot4 <- ggplot(data=ejemplo)+
  geom_line(size=1,aes(x=edad,y=DP,  linetype=as.factor(tipo), 
                       color=as.factor(parcela)))+
  geom_point(aes(x=edad,y=DP, shape=as.factor(parcela), color=as.factor(parcela)))+
  labs(fill="",color="Parcela",shape="Parcela", linetype= "Dato")+
  geom_vline(xintercept = 40, linetype="dashed", color = "red", size=1)+
  theme_bw()+ labs(x="Edad (años)", y = "Diámetro normal promedio (cm)")+
  #scale_x_continuous(breaks = seq(5,25, by=5))+
  #scale_y_continuous(limits = c(-5,5))+
  theme(axis.text = element_text(size = 12, color = "black"),
        legend.position = c(.15, 0.85), 
        legend.text = element_text(color = "black", size=8, face = "bold"),
        legend.background = element_rect(fill = "transparent"),
        panel.border = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(0,0,0,0, unit="cm"));plot4

# ========================================================================================
cowplot::plot_grid(plot4,
          plot2,                                    # Create grid of plots
          plot3, 
          ncol = 3,
          hjust = -5,
          labels = c("a)", "b)", "c)"))

ggsave("C:/Users/Nava/Desktop/Doctorado COLPOS/Herramientas silvicolas/Plot2.tiff", 
       dpi=300, height=15, width=32, units="cm")

# ========================================================================================
dn <- 
  read_excel("C:/Users/Nava/Desktop/Doctorado COLPOS/Herramientas silvicolas/Parcelas ejemplo.xlsx", 
             sheet = "resumen", range = "I1:L145") 

ggplot(data=dn)+
  geom_point(aes(x=cd,y=na),size=0.9)+
  geom_line(size=0.6,aes(x=cd,y=na,linetype=factor(edad),color=factor(edad)))+
  facet_grid(.~parcela)+
  scale_x_continuous(breaks = seq(0,55,by=5))+
  scale_y_continuous(limits = c(0,1500),expand = c(0, 0))+
  theme_bw()+
  labs(x = expression("Categoría diamétrica"~"(cm)"),
       y = expression("Número de árboles"~(ha^-1)),color="",linetype="")+
  theme(strip.text = element_text(size=10, face="bold"),
        axis.text = element_text(family="Times", colour = "black",size = 12),
        legend.position =c(0.04,0.88),
        legend.background = element_rect(fill="transparent"),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(0,0,0,0, unit="cm"))

ggsave("C:/Users/Nava/Desktop/Doctorado COLPOS/Herramientas silvicolas/Plot3.tiff", 
       dpi=300, height=15, width=32, units="cm")
