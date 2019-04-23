library(cowplot)
library(ggplot2)
library(dplyr)
library(tidyr)


setwd('C:/Users/mmorenozam/Documents/MyPapers/1.cbf_v1.0/2.outputs/1.main')

load('m5_b1.Rsave')

T = 13
x0 = c(55.482, 49.669, 0, 0, 1.379, 0.175424909,0.003047264,2.86522E-06)
t0 = 0
ts = c(6,12,24,30,36,48,54,60,72,84,96,120,144)

x = structure(c(26.802,16.311,13.706,13.248,7.54,1.278,2.694,3.08,0.757,2.465,2.673,3.183,7.631,
                31.959,18.937,19.238,15.217,10.351,0.809,1.007,1.205,0.757,1.996,2.673,3.183,2.287,
                0.201,2.324,2.46,1.996,3.173,4.025,6.033,5.685,3.605,1.617,0.576,0.964,1.56,
                0.148,0.342,0.548,1.033,0.658,4.873,3.194,2.694,2.72,1.928,2.482,1.883,1.63,
                0.963,1.216,1.467,1.996,1.211,3.333,6.033,4.323,8.685,5.658,11.935,11.354,11.511,
                0.119149235,0.212369067,0.068721283,0.045508368,0.052250597,0.035488795,0.020468747,0.03750518,0.004509114,0.00048539,0.000190586,2.91133E-06,1.45912E-06,
                0.006575216,0.02528774,0.37403308,0.364678377,0.36383964,0.073945204,0.019182712,0.027410062,0.001068833,0.00101137,0.023873166,0.00458047,0.000150977,
                7.71184E-06,7.09836E-06,1.0427E-05,7.90966E-06,1.10194E-05,6.05561E-05,0.000127104,0.000700097,0.000130665,0.000127104,0.005347589,0.008514478,3.38188E-05),
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
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,60,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,60))

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
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,60,by=10),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,60))

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
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,7,by=1),sec.axis = dup_axis(labels=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,7))

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
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.05,16))

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
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,0.5,by=0.1),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.001,0.5))


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
  scale_y_continuous(expand = c(0, 0),breaks= seq(0,0.01,by=0.002),sec.axis = dup_axis(labels=NULL,name=NULL)) + 
  coord_cartesian(xlim=c(-0.05,148),ylim=c(-0.0001,0.01))

box1 = plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4,align='hv',labels=c("(a)","(b)","(c)","(d)","(e)","(f)","(g)","(h)"),
                 label_size = 18)

#box1
save_plot('fig5.png',box1,base_height = 8.3,base_width = 17.2)


