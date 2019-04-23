library(cowplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rstan)
library(latex2exp)

getSimsTable <- function(x, ...){#function taken from the talk of Charles Margossian, StanCon2017
  require(dplyr)
  nChains <- dim(x)[2]
  nPost <- dim(x)[1]
  x %>%
    as.data.frame(...) %>%
    mutate(chain = rep(1:nChains, ea = nPost),
           iteration = rep(1:nPost, nChains))
}


setwd('C:/Users/mmorenozam/Documents/MyPapers/1.cbf_v1.0/2.outputs/1.main') # where the outputs are


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




load('m5_b1.Rsave')
pb1 = getSimsTable(fit)
nm = names(pb1)
nm = nm[2:25]

glu = 55.482000000
fru = 49.669000000
eto = 6.033000000
lac = 4.873000000
acc = 11.935000000
yst =  10^7.151*0.000000015  
lab = 10^8.476 *0.00000000125
aab =  10^7.483*0.00000000028
pb1$yc1 = pb1$yc1*(glu/yst)
pb1$yc2 = pb1$yc2*(glu/lab)
pb1$yc3 = pb1$yc3*(fru/yst)
pb1$yc4 = pb1$yc4*(eto/yst)
pb1$yc5 = pb1$yc5*(eto/yst)
pb1$yc6 = pb1$yc6*(eto/aab)
pb1$yc7 = pb1$yc7*(lac/lab)
pb1$yc8 = pb1$yc8*(lac/aab)
pb1$yc9 = pb1$yc9*(acc/lab)
pb1$yc10 = pb1$yc10*(acc/aab)
pb1$yc11 = pb1$yc11*(acc/aab)
pb1$ks1 = pb1$ks1*glu
pb1$ks2 = pb1$ks2*fru
pb1$ks3 = pb1$ks3*glu
pb1$ks4 = pb1$ks4*eto
pb1$ks5 = pb1$ks5*(lac/aab)
pb1$k1 = pb1$k1/eto
pb1$k2 = pb1$k2/lac
pb1$k3 = pb1$k3/(acc^2)



load('m5_b2.Rsave')
pb2 = getSimsTable(fit)

glu = 42.93600000
fru = 67.24900000
eto = 7.05700000 
lac = 4.12300000
acc = 18.62500000
yst = 10^7.210*0.000000015 
lab =  10^8.748*0.00000000125 
aab =  10^7.984*0.00000000028
pb2$yc1 = pb2$yc1*(glu/yst)
pb2$yc2 = pb2$yc2*(glu/lab)
pb2$yc3 = pb2$yc3*(fru/yst)
pb2$yc4 = pb2$yc4*(eto/yst)
pb2$yc5 = pb2$yc5*(eto/yst)
pb2$yc6 = pb2$yc6*(eto/aab)
pb2$yc7 = pb2$yc7*(lac/lab)
pb2$yc8 = pb2$yc8*(lac/aab)
pb2$yc9 = pb2$yc9*(acc/lab)
pb2$yc10 = pb2$yc10*(acc/aab)
pb2$yc11 = pb2$yc11*(acc/aab)
pb2$ks1 = pb2$ks1*glu
pb2$ks2 = pb2$ks2*fru
pb2$ks3 = pb2$ks3*glu
pb2$ks4 = pb2$ks4*eto
pb2$ks5 = pb2$ks5*(lac/aab)
pb2$k1 = pb2$k1/eto
pb2$k2 = pb2$k2/lac
pb2$k3 = pb2$k3/(acc^2)

pb1$tr = '1'
pb2$tr = '2'

pb = rbind(pb1,pb2)

p = list()
for(i in seq_along(nm)){
  p[[i]]=ggplot(pb, aes_string(x="tr",y=nm[i]))+
    geom_violin(fill="#0073C2FF",colour='grey')+
    labs(y = TeX(par[i]), x = "box")+
    theme(axis.title=element_text(size=18),
          axis.text=element_text(size=14))
}

plot1 = plot_grid(p[[1]],p[[2]],p[[3]],p[[4]],
                p[[5]],p[[6]],p[[7]],p[[8]],
                p[[9]],p[[10]],p[[11]],p[[12]],
                p[[13]],p[[14]],p[[15]],p[[16]],
                p[[17]],p[[18]],p[[19]],p[[20]],
                p[[21]],p[[22]],p[[23]],p[[24]],
                ncol=6,align='hv',
                label_size = 22)

save_plot('fig7.png',plot1,base_height = 12,base_width = 24)
