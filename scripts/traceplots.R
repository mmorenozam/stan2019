library(cowplot)
library(ggplot2)
library(rstan)
library(latex2exp)
library(ggsci)



setwd('C:/Users/mmorenozam/Documents/MyPapers/1.cbf_v1.0/2.outputs/1.main') 

#in the following line, change for the name of the output of which one wants the traceplots
load('m5_b2.RSave')

getSimsTable <- function(x, ...){#function taken from the talk of Charles Margossian, StanCon2017
  require(dplyr)
  nChains <- dim(x)[2]
  nPost <- dim(x)[1]
  x %>%
    as.data.frame(...) %>%
    mutate(chain = rep(1:nChains, ea = nPost),
           iteration = rep(1:nPost, nChains))
}

data = getSimsTable(fit)

mu1 = '$\\mu_{max}^{Y_{Glc}}$'
mu2 = '$\\mu_{max}^{Y_{Fru}}$' 
mu3 = '$\\mu_{max}^{LAB}$' 
mu4 = '$\\mu_{max}^{AAB_{EtOH}}$' 
mu5 = '$\\mu_{max}^{AAB_{LA}}$' 
ks1 = '$K_{Glc}^{Y}$'
ks2 = '$K_{Fru}^{Y}$'
ks3 = '$K_{Glc}^{LAB}$'
ks4 = '$K_{EtOH}^{AAB}$'
ks5 = '$K_{LA}^{AAB}$'
k1 = '$k_{Y}$'
k2 = '$k_{LAB}$'
k3 = '$k_{AAB}$'
y1 = '$Y_{Glc|Y}$'
y2 = '$Y_{Glc|LAB}$'
y3 = '$Y_{Fru|Y}$'
y4 = '$Y^{Glc}_{EtOH|Y}$'
y5 = '$Y^{Fru}_{EtOH|Y}$'
y6 = '$Y_{EtOH|AAB}$'
y7 = '$Y_{LA|LAB}$'
y8 = '$Y_{LA|AAB}$'
y9 = '$Y_{Ac|LAB}$'
y10 = '$Y^{EtOH}_{Ac|AAB}$'
y11 = '$Y^{LA}_{Ac|AAB}$'
par = c(mu1,mu2,mu3,mu4,mu5,ks1,ks2,ks3,ks4,ks5,k1,k2,k3,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11)

data = names(data)
data = data[2:25]

p = list()
for (i in seq_along(data)){
  p[[i]] = traceplot(fit,pars=data[i],inc_warmup=T,size=0.01,alpha=0.3)+ ylab(TeX(par[i]))+
    theme(axis.text=element_text(size=5),axis.title=element_text(size=10),legend.position="none",
          axis.line.x = element_line( size = 0.01),axis.line.y = element_line( size = 0.01),
          axis.ticks = element_line( size = 0.01))+
    scale_color_jco()
}

plot = plot_grid(p[[1]],p[[2]],p[[3]],p[[4]],
                 p[[5]],p[[6]],p[[7]],p[[8]],
                 p[[9]],p[[10]],p[[11]],p[[12]],
                 p[[13]],p[[14]],p[[15]],p[[16]],
                 p[[17]],p[[18]],p[[19]],p[[20]],
                 p[[21]],p[[22]],p[[23]],p[[24]],
                 ncol=4,align='hv')

save_plot('fig4.png',plot,base_height = 6,base_width = 10)
