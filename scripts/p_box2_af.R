library(cowplot)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd('C:/Users/mmorenozam/Documents/MyPapers/1.cbf_v1.0/2.outputs/1.main')

load('m5_b2.Rsave')


T = 13
x0 = c(42.936,67.249,0.318,0,0.942,0.004754351,0.000221264,3.86508E-06)
t0 = 0
ts = c(6,12,24,30,36,48,54,60,72,84,96,120,144)

x = structure(c(23.452,8.264,4.85,1.27,1.62,2.32,0.294,1.467,1.892,5.975,7.863,6.521,7.19,
                21.349,14.478,10.974,2.641,0.889,4.057,1.756,1.467,3.812,8.351,9.416,9.081,9.109,
                0.386,2.211,4.496,4.912,6.714,4.307,7.057,4.097,1.044,1.642,0.9,0.341,0.29,
                0.165,0.679,1.75,0.393,0.823,0.992,1.161,0.968,1.609,4.123,2.476,2.565,1.642,
                1.079,1.194,2.532,1.329,2.183,1.718,2.272,2.733,6.568,11.767,9.083,18.625,17.187,
                0.069998907,0.243271515,0.037852212,0.022340416,0.038823194,0.015491421,0.055474227,0.000444725,0.001542024,7.90845E-05,0.00016676,0.000144574,9.48618E-06,
                0.002753658,0.026786133,0.016669018,0.051749959,0.125866459,0.54564479,0.699697002,0.05151219,0.11800761,0.013928682,0.024149604,0.002923547,0.001451811,
                8.37834E-06,1.41957E-05,2.74889E-05,4.53062E-05,9.06062E-06,3.61541E-05,0.002723692,0.001033137,0.001436012,6.57897E-05,0.026987213,0.00468984,9.33594E-06),
              .Dim=c(13,8))

x1 = rbind(x0,x)
scl = c(max(x1[,1]),max(x1[,2]),max(x1[,3]),max(x1[,4]),max(x1[,5]),max(x1[,6]),max(x1[,7]),max(x1[,8]))



pred = as.data.frame(fit, pars = "x_hat") %>%
  gather(factor_key = TRUE) %>%
  group_by(key) %>%
  summarize(lb = quantile(value, probs = 0.025),
            median = quantile(value, probs = 0.5),
            ub = quantile(value, probs = 0.975)) 

sp = split(pred,rep(1:8,each=T))

for (i in 1:8){
  sp[i] = lapply(sp[i],"*",scl[i])
  sp[i] = mapply(cbind, sp[i], "obs"=list(x[,i]), SIMPLIFY=F)
  sp[i] = mapply(cbind, sp[i], "time"=list(ts), SIMPLIFY=F)
  sp[i] = mapply(rbind,list(c(NA,x0[i],x0[i],x0[i],x0[i],0)),sp[i],SIMPLIFY=F)
}

hali = 0.96

p1 = ggplot(as.data.frame(sp[1]), aes(x = X1.time, y = X1.obs)) +
  geom_point() + ylab(expression(paste('mg g(pulp) '^{-1}))) + xlab('') + #ggtitle('Glu')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+ 
  geom_line(aes(x = X1.time, y = X1.median),color='red',size=0.70) +
  geom_line(aes(x = X1.time, y = X1.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X1.time, y = X1.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL)) +
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,50,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,50))

p2 = ggplot(as.data.frame(sp[2]), aes(x = X2.time, y = X2.obs)) +
  geom_point() + ylab('') + xlab('') + #ggtitle('Fru')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+ 
  geom_line(aes(x = X2.time, y = X2.median),color='red',size=0.70) +
  geom_line(aes(x = X2.time, y = X2.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X2.time, y = X2.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL)) + 
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,70,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,70))

p3 = ggplot(as.data.frame(sp[3]), aes(x = X3.time, y = X3.obs)) +
  geom_point() + ylab('') + xlab('') + #ggtitle('EtOH')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+ 
  geom_line(aes(x = X3.time, y = X3.median),color='red',size=0.70) +
  geom_line(aes(x = X3.time, y = X3.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X3.time, y = X3.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL)) + 
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,8,by=1),sec.axis = dup_axis(labels=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,8))

p4 = ggplot(as.data.frame(sp[4]), aes(x = X4.time, y = X4.obs)) +
  geom_point() + ylab('') + xlab('') + #ggtitle('LA')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+ 
  geom_line(aes(x = X4.time, y = X4.median),color='red',size=0.70) +
  geom_line(aes(x = X4.time, y = X4.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X4.time, y = X4.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL)) + 
  scale_y_continuous(expand = c(0, 0),sec.axis = dup_axis(labels=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,5))

p5 = ggplot(as.data.frame(sp[5]), aes(x = X5.time, y = X5.obs)) +
  geom_point() + ylab(expression(paste('mg g(pulp) '^{-1}))) + xlab('time (h)') + #ggtitle('Ac')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+ 
  geom_line(aes(x = X5.time, y = X5.median),color='red',size=0.70) +
  geom_line(aes(x = X5.time, y = X5.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X5.time, y = X5.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  scale_y_continuous(expand = c(0, 0),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,22))

p6 = ggplot(as.data.frame(sp[6]), aes(x = X6.time, y = X6.obs)) +
  geom_point() + ylab('') + xlab('time (h)') + #ggtitle('Y')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+ 
  geom_line(aes(x = X6.time, y = X6.median),color='red',size=0.70) +
  geom_line(aes(x = X6.time, y = X6.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X6.time, y = X6.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,0.3,by=0.1),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.001,0.3))

p7 = ggplot(as.data.frame(sp[7]), aes(x = X7.time, y = X7.obs)) +
  geom_point() + ylab('') + xlab('time (h)') + #ggtitle('LAB')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+ 
  geom_line(aes(x = X7.time, y = X7.median),color='red',size=0.70) +
  geom_line(aes(x = X7.time, y = X7.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X7.time, y = X7.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,0.75,by=0.15),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.001,0.75))


p8 = ggplot(as.data.frame(sp[8]), aes(x = X8.time, y = X8.obs)) +
  geom_point() + ylab('') + xlab('time (h)') + #ggtitle('AAB')+
  theme(text = element_text(size = 16), axis.text = element_text(size = 14),
        legend.position = "none", strip.text = element_text(size = 14),
        plot.title = element_text(hjust = hali,margin = margin(t = 15, b = -50), size = 20),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour='black'),
        plot.margin = unit(c(0.1,0.1,0.1,0.1),'cm'))+  
  geom_line(aes(x = X8.time, y = X8.median),color='red',size=0.70) +
  geom_line(aes(x = X8.time, y = X8.ub),color='red',linetype='dashed') +
  geom_line(aes(x = X8.time, y = X8.lb),color='red',linetype='dashed') +
  scale_x_continuous(expand = c(0, 0),breaks= seq(0,144,by=24),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,0.03,by=0.006),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.0001,0.03))

box2 = plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4,align='hv',labels=c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)"),
                 label_size = 18)


save_plot('fig6.png',box2,base_height = 8.3,base_width = 17.2)



