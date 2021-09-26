library("deSolve")
library('ggplot2')
library('ggpubr')
library('plotly')
library('rbokeh')
library('stringdist')
#options ( digits  =  20 ) 

demography<-read.csv('demography.csv',header = TRUE)
colnames(demography)<-c('Country','0-18','18-40','40-60','60+','Total','Ratio 0-18','Ratio 18-40','Ratio 40-60','Ratio 60+')
correct_names<-names(opt_country)
rownames(demography)<-correct_names[amatch(demography$Country, correct_names, maxDist=Inf, method = 'jw')]

####### original one
# covid <- function(t, y, p) {
#   with(as.list(c(y, p)), {
#     dsus_a1 <- -r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1)/pop_a1 -r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1)/pop_a2 -r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1)/pop_a3 -r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1)/pop_a4
#     dexp_a1 <- -(r3/(r3*5.2-1))*exp_a1 + r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1)/pop_a1 +r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1)/pop_a2 +r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1)/pop_a3 +r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1)/pop_a4 
#     dci_a1 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a1 - r3*ci_a1 
#     dcr_a1 <- alpha*(r3/(r3*5.2 -1))*exp_a1 - cr_a1*(2*r3*r4/(2*r4+r3))
#     di_a1 <- mu*r3*ci_a1-r11*i_a1
#     dix_a1 <- (1-mu)*r3*ci_a1-r4*ix_a1
#     did_a1 <- cfr_a1*r11*i_a1-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a1
#     dir_a1 <- (1-cfr_a1)*r11*i_a1-r12*ir_a1
#     drx_a1 <- cr_a1*(2*r3*r4/(2*r4+r3))+r4*ix_a1
#     drz_a1 <- r12*ir_a1
#     ddead_a1 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a1
#     dsus_a1v <- -r1_a1v*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1v)/pop_a1 -r1_a1v*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1v)/pop_a2 -r1_a1v*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1v)/pop_a3 -r1_a1v*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1v)/pop_a4 
#     dexp_a1v <- -(r3/(r3*5.2 -1))*exp_a1v + r1_a1v*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1v)/pop_a1 +r1_a1v*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1v)/pop_a2 +r1_a1v*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1v)/pop_a3 +r1_a1v*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1v)/pop_a4  
#     dc_a1v <- (r3/(r3*5.2 -1))*exp_a1v - r3*c_a1v 
#     di_a1v <- muv*r3*c_a1v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1v 
#     dix_a1v <- (1-muv)*r3*c_a1v-r4*ix_a1v 
#     drx_a1v <- ix_a1v*r4 
#     ddead_a1v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1v 
#     dsus_a1fv <- -r1_a1fv*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1fv)/pop_a1 -r1_a1fv*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1fv)/pop_a2 -r1_a1fv*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1fv)/pop_a3 -r1_a1fv*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1fv)/pop_a4 
#     dexp_a1fv <- -(r3/(r3*5.2 -1))*exp_a1fv + r1_a1fv*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1fv)/pop_a1 +r1_a1fv*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1fv)/pop_a2 +r1_a1fv*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1fv)/pop_a3 +r1_a1fv*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1fv)/pop_a4 
#     dc_a1fv <- (r3/(r3*5.2 -1))*exp_a1fv - r3*c_a1fv 
#     di_a1fv <- mufv*r3*c_a1fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1fv 
#     dix_a1fv <- (1-mufv)*r3*c_a1fv-r4*ix_a1fv 
#     drx_a1fv <- ix_a1fv*r4 
#     ddead_a1fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1fv
#     dsus_a2 <- -r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2)/pop_a1 -r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2)/pop_a2 -r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2)/pop_a3 -r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2)/pop_a4 
#     dexp_a2 <- -(r3/(r3*5.2 -1))*exp_a2 + r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2)/pop_a1 +r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2)/pop_a2 +r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2)/pop_a3 +r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2)/pop_a4  
#     dci_a2 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a2 - r3*ci_a2 
#     dcr_a2 <- alpha*(r3/(r3*5.2 -1))*exp_a2 - cr_a2*(2*r3*r4/(2*r4+r3)) 
#     di_a2 <- mu*r3*ci_a2-r11*i_a2 
#     dix_a2 <- (1-mu)*r3*ci_a2-r4*ix_a2 
#     did_a2 <- cfr_a2*r11*i_a2-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a2 
#     dir_a2 <- (1-cfr_a2)*r11*i_a2-r12*ir_a2 
#     drx_a2 <- cr_a2*(2*r3*r4/(2*r4+r3))+r4*ix_a2 
#     drz_a2 <- r12*ir_a2 
#     ddead_a2 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a2 
#     dsus_a2v <- -r1_a2v*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2v)/pop_a1 -r1_a2v*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2v)/pop_a2 -r1_a2v*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2v)/pop_a3 -r1_a2v*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2v)/pop_a4 
#     dexp_a2v <- -(r3/(r3*5.2 -1))*exp_a2v + r1_a2v*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2v)/pop_a1 +r1_a2v*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2v)/pop_a2 +r1_a2v*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2v)/pop_a3 +r1_a2v*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2v)/pop_a4 
#     dc_a2v <- (r3/(r3*5.2 -1))*exp_a2v - r3*c_a2v 
#     di_a2v <- muv*r3*c_a2v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2v 
#     dix_a2v <- (1-muv)*r3*c_a2v-r4*ix_a2v 
#     drx_a2v <- ix_a2v*r4 
#     ddead_a2v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2v 
#     dsus_a2fv <- -r1_a2fv*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2fv)/pop_a1 -r1_a2fv*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2fv)/pop_a2 -r1_a2fv*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2fv)/pop_a3 -r1_a2fv*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2fv)/pop_a4 
#     dexp_a2fv <- -(r3/(r3*5.2 -1))*exp_a2fv + r1_a2fv*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2fv)/pop_a1 +r1_a2fv*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2fv)/pop_a2 +r1_a2fv*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2fv)/pop_a3 +r1_a2fv*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2fv)/pop_a4  
#     dc_a2fv <- (r3/(r3*5.2 -1))*exp_a2fv - r3*c_a2fv 
#     di_a2fv <- mufv*r3*c_a2fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2fv 
#     dix_a2fv <- (1-mufv)*r3*c_a2fv-r4*ix_a2fv 
#     drx_a2fv <- ix_a2fv*r4 
#     ddead_a2fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2fv
#     dsus_a3 <- -r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3)/pop_a1 -r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3)/pop_a2 -r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3)/pop_a3 -r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3)/pop_a4 
#     dexp_a3 <- -(r3/(r3*5.2 -1))*exp_a3  + r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3)/pop_a1 +r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3)/pop_a2 +r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3)/pop_a3 +r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3)/pop_a4 
#     dci_a3 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a3 - r3*ci_a3 
#     dcr_a3 <- alpha*(r3/(r3*5.2 -1))*exp_a3 - cr_a3*(2*r3*r4/(2*r4+r3)) 
#     di_a3 <- mu*r3*ci_a3-r11*i_a3 
#     dix_a3 <- (1-mu)*r3*ci_a3-r4*ix_a3 
#     did_a3 <- cfr_a3*r11*i_a3-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a3 
#     dir_a3 <- (1-cfr_a3)*r11*i_a3-r12*ir_a3 
#     drx_a3 <- cr_a3*(2*r3*r4/(2*r4+r3))+r4*ix_a3 
#     drz_a3 <- r12*ir_a3 
#     ddead_a3 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a3 
#     dsus_a3v <- -r1_a3v*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3v)/pop_a1 -r1_a3v*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3v)/pop_a2 -r1_a3v*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3v)/pop_a3 -r1_a3v*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3v)/pop_a4 
#     dexp_a3v <- -(r3/(r3*5.2 -1))*exp_a3v + r1_a3v*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3v)/pop_a1 +r1_a3v*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3v)/pop_a2 +r1_a3v*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3v)/pop_a3 +r1_a3v*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3v)/pop_a4  
#     dc_a3v <- (r3/(r3*5.2 -1))*exp_a3v - r3*c_a3v 
#     di_a3v <- muv*r3*c_a3v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3v 
#     dix_a3v <- (1-muv)*r3*c_a3v-r4*ix_a3v 
#     drx_a3v <- ix_a3v*r4 
#     ddead_a3v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3v 
#     dsus_a3fv <- -r1_a3fv*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3fv)/pop_a1 -r1_a3fv*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3fv)/pop_a2 -r1_a3fv*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3fv)/pop_a3 -r1_a3fv*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3fv)/pop_a4 
#     dexp_a3fv <- -(r3/(r3*5.2 -1))*exp_a3fv + r1_a3fv*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3fv)/pop_a1 +r1_a3fv*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3fv)/pop_a2 +r1_a3fv*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3fv)/pop_a3 +r1_a3fv*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3fv)/pop_a4  
#     dc_a3fv <- (r3/(r3*5.2 -1))*exp_a3fv - r3*c_a3fv 
#     di_a3fv <- mufv*r3*c_a3fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3fv 
#     dix_a3fv <- (1-mufv)*r3*c_a3fv-r4*ix_a3fv 
#     drx_a3fv <- ix_a3fv*r4 
#     ddead_a3fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3fv
#     dsus_a4 <- -r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4)/pop_a1 -r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4)/pop_a2 -r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4)/pop_a3 -r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4)/pop_a4 
#     dexp_a4 <- -(r3/(r3*5.2 -1))*exp_a4  + r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4)/pop_a1 +r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4)/pop_a2 +r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4)/pop_a3 +r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4)/pop_a4 
#     dci_a4 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a4 - r3*ci_a4 
#     dcr_a4 <- alpha*(r3/(r3*5.2 -1))*exp_a4 - cr_a4*(2*r3*r4/(2*r4+r3)) 
#     di_a4 <- mu*r3*ci_a4-r11*i_a4 
#     dix_a4 <- (1-mu)*r3*ci_a4-r4*ix_a4 
#     did_a4 <- cfr_a4*r11*i_a4-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a4 
#     dir_a4 <- (1-cfr_a4)*r11*i_a4-r12*ir_a4 
#     drx_a4 <- cr_a4*(2*r3*r4/(2*r4+r3))+r4*ix_a4 
#     drz_a4 <- r12*ir_a4 
#     ddead_a4 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a4 
#     dsus_a4v <- -r1_a4v*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4v)/pop_a1 -r1_a4v*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4v)/pop_a2 -r1_a4v*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4v)/pop_a3 -r1_a4v*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4v)/pop_a4 
#     dexp_a4v <- -(r3/(r3*5.2 -1))*exp_a4v + r1_a4v*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4v)/pop_a1 +r1_a4v*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4v)/pop_a2 +r1_a4v*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4v)/pop_a3 +r1_a4v*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4v)/pop_a4  
#     dc_a4v <- (r3/(r3*5.2 -1))*exp_a4v - r3*c_a4v 
#     di_a4v <- muv*r3*c_a4v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4v 
#     dix_a4v <- (1-muv)*r3*c_a4v-r4*ix_a4v 
#     drx_a4v <- ix_a4v*r4 
#     ddead_a4v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4v 
#     dsus_a4fv <- -r1_a4fv*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4fv)/pop_a1 -r1_a4fv*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4fv)/pop_a2 -r1_a4fv*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4fv)/pop_a3 -r1_a4fv*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4fv)/pop_a4 
#     dexp_a4fv <- -(r3/(r3*5.2 -1))*exp_a4fv + r1_a4fv*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4fv)/pop_a1 +r1_a4fv*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4fv)/pop_a2 +r1_a4fv*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4fv)/pop_a3 +r1_a4fv*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4fv)/pop_a4  
#     dc_a4fv <- (r3/(r3*5.2 -1))*exp_a4fv - r3*c_a4fv 
#     di_a4fv <- mufv*r3*c_a4fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4fv 
#     dix_a4fv <- (1-mufv)*r3*c_a4fv-r4*ix_a4fv 
#     drx_a4fv <- ix_a4fv*r4 
#     ddead_a4fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4fv
#     
#     return(list(c(sus_a1=dsus_a1, exp_a1=dexp_a1, ci_a1=dci_a1, cr_a1=dcr_a1, i_a1=di_a1, 
#            ix_a1=dix_a1, id_a1=did_a1, ir_a1=dir_a1, rx_a1=drx_a1, rz_a1=drz_a1, 
#            dead_a1=ddead_a1, sus_a1v=dsus_a1v, exp_a1v=dexp_a1v, c_a1v=dc_a1v, i_a1v=di_a1v, 
#            ix_a1v=dix_a1v, rx_a1v=drx_a1v, dead_a1v=ddead_a1v, sus_a1fv=dsus_a1fv, exp_a1fv=dexp_a1fv, 
#            c_a1fv=dc_a1fv, i_a1fv=di_a1fv, ix_a1fv=dix_a1fv, rx_a1fv=drx_a1fv, dead_a1fv=ddead_a1fv,
#            sus_a2=dsus_a2, exp_a2=dexp_a2, ci_a2=dci_a2, cr_a2=dcr_a2, i_a2=di_a2, 
#            ix_a2=dix_a2, id_a2=did_a2, ir_a2=dir_a2, rx_a2=drx_a2, rz_a2=drz_a2, 
#            dead_a2=ddead_a2, sus_a2v=dsus_a2v, exp_a2v=dexp_a2v, c_a2v=dc_a2v, i_a2v=di_a2v, 
#            ix_a2v=dix_a2v, rx_a2v=drx_a2v, dead_a2v=ddead_a2v, sus_a2fv=dsus_a2fv, exp_a2fv=dexp_a2fv, 
#            c_a2fv=dc_a2fv, i_a2fv=di_a2fv, ix_a2fv=dix_a2fv, rx_a2fv=drx_a2fv, dead_a2fv=ddead_a2fv,
#            sus_a3=dsus_a3, exp_a3=dexp_a3, ci_a3=dci_a3, cr_a3=dcr_a3, i_a3=di_a3, 
#            ix_a3=dix_a3, id_a3=did_a3, ir_a3=dir_a3, rx_a3=drx_a3, rz_a3=drz_a3, 
#            dead_a3=ddead_a3, sus_a3v=dsus_a3v, exp_a3v=dexp_a3v, c_a3v=dc_a3v, i_a3v=di_a3v, 
#            ix_a3v=dix_a3v, rx_a3v=drx_a3v, dead_a3v=ddead_a3v, sus_a3fv=dsus_a3fv, exp_a3fv=dexp_a3fv, 
#            c_a3fv=dc_a3fv, i_a3fv=di_a3fv, ix_a3fv=dix_a3fv, rx_a3fv=drx_a3fv, dead_a3fv=ddead_a3fv,
#            sus_a4=dsus_a4, exp_a4=dexp_a4, ci_a4=dci_a4, cr_a4=dcr_a4, i_a4=di_a4, 
#            ix_a4=dix_a4, id_a4=did_a4, ir_a4=dir_a4, rx_a4=drx_a4, rz_a4=drz_a4, 
#            dead_a4=ddead_a4, sus_a4v=dsus_a4v, exp_a4v=dexp_a4v, c_a4v=dc_a4v, i_a4v=di_a4v, 
#            ix_a4v=dix_a4v, rx_a4v=drx_a4v, dead_a4v=ddead_a4v, sus_a4fv=dsus_a4fv, exp_a4fv=dexp_a4fv, 
#            c_a4fv=dc_a4fv, i_a4fv=di_a4fv, ix_a4fv=dix_a4fv, rx_a4fv=drx_a4fv, dead_a4fv=ddead_a4fv
#     )))
#   })
# }




covid <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dsus_a1 <- -r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1)/pop_a1 -r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1)/pop_a2 -r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1)/pop_a3 -r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1)/pop_a4
    dexp_a1 <- -(r3/(r3*5.2-1))*exp_a1 + r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1)/pop_a1 +r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1)/pop_a2 +r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1)/pop_a3 +r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1)/pop_a4 
    dci_a1 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a1 - r3*ci_a1 
    dcr_a1 <- alpha*(r3/(r3*5.2 -1))*exp_a1 - cr_a1*(2*r3*r4/(2*r4+r3))
    di_a1 <- mu*r3*ci_a1-r11*i_a1
    dix_a1 <- (1-mu)*r3*ci_a1-r4*ix_a1
    did_a1 <- cfr_a1*r11*i_a1-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a1
    dir_a1 <- (1-cfr_a1)*r11*i_a1-r12*ir_a1
    drx_a1 <- cr_a1*(2*r3*r4/(2*r4+r3))+r4*ix_a1
    drz_a1 <- r12*ir_a1
    ddead_a1 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a1
    dsus_a1v <- -r1_a1v*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1v)/pop_a1 -r1_a1v*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1v)/pop_a2 -r1_a1v*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1v)/pop_a3 -r1_a1v*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1v)/pop_a4 
    dexp_a1v <- -(r3/(r3*5.2 -1))*exp_a1v + r1_a1v*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1v)/pop_a1 +r1_a1v*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1v)/pop_a2 +r1_a1v*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1v)/pop_a3 +r1_a1v*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1v)/pop_a4  
    dc_a1v <- (r3/(r3*5.2 -1))*exp_a1v - r3*c_a1v 
    di_a1v <- muv*r3*c_a1v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1v 
    dix_a1v <- (1-muv)*r3*c_a1v-r4*ix_a1v 
    drx_a1v <- ix_a1v*r4 
    ddead_a1v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1v 
    dsus_a1fv <- -r1_a1fv*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1fv)/pop_a1 -r1_a1fv*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1fv)/pop_a2 -r1_a1fv*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1fv)/pop_a3 -r1_a1fv*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1fv)/pop_a4 
    dexp_a1fv <- -(r3/(r3*5.2 -1))*exp_a1fv + r1_a1fv*r1_a1*zcm11*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a1fv)/pop_a1 +r1_a1fv*r1_a1*zcm12*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a1fv)/pop_a2 +r1_a1fv*r1_a1*zcm13*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a1fv)/pop_a3 +r1_a1fv*r1_a1*zcm14*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a1fv)/pop_a4 
    dc_a1fv <- (r3/(r3*5.2 -1))*exp_a1fv - r3*c_a1fv 
    di_a1fv <- mufv*r3*c_a1fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1fv 
    dix_a1fv <- (1-mufv)*r3*c_a1fv-r4*ix_a1fv 
    drx_a1fv <- ix_a1fv*r4 
    ddead_a1fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a1fv
    dsus_a2 <- -r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2)/pop_a1 -r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2)/pop_a2 -r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2)/pop_a3 -r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2)/pop_a4 
    dexp_a2 <- -(r3/(r3*5.2 -1))*exp_a2 + r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2)/pop_a1 +r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2)/pop_a2 +r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2)/pop_a3 +r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2)/pop_a4  
    dci_a2 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a2 - r3*ci_a2 
    dcr_a2 <- alpha*(r3/(r3*5.2 -1))*exp_a2 - cr_a2*(2*r3*r4/(2*r4+r3)) 
    di_a2 <- mu*r3*ci_a2-r11*i_a2 
    dix_a2 <- (1-mu)*r3*ci_a2-r4*ix_a2 
    did_a2 <- cfr_a2*r11*i_a2-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a2 
    dir_a2 <- (1-cfr_a2)*r11*i_a2-r12*ir_a2 
    drx_a2 <- cr_a2*(2*r3*r4/(2*r4+r3))+r4*ix_a2 
    drz_a2 <- r12*ir_a2 
    ddead_a2 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a2 
    dsus_a2v <- -r1_a2v*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2v)/pop_a1 -r1_a2v*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2v)/pop_a2 -r1_a2v*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2v)/pop_a3 -r1_a2v*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2v)/pop_a4 
    dexp_a2v <- -(r3/(r3*5.2 -1))*exp_a2v + r1_a2v*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2v)/pop_a1 +r1_a2v*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2v)/pop_a2 +r1_a2v*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2v)/pop_a3 +r1_a2v*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2v)/pop_a4 
    dc_a2v <- (r3/(r3*5.2 -1))*exp_a2v - r3*c_a2v 
    di_a2v <- muv*r3*c_a2v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2v 
    dix_a2v <- (1-muv)*r3*c_a2v-r4*ix_a2v 
    drx_a2v <- ix_a2v*r4 
    ddead_a2v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2v 
    dsus_a2fv <- -r1_a2fv*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2fv)/pop_a1 -r1_a2fv*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2fv)/pop_a2 -r1_a2fv*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2fv)/pop_a3 -r1_a2fv*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2fv)/pop_a4 
    dexp_a2fv <- -(r3/(r3*5.2 -1))*exp_a2fv + r1_a2fv*r1_a2*zcm21*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a2fv)/pop_a1 +r1_a2fv*r1_a2*zcm22*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a2fv)/pop_a2 +r1_a2fv*r1_a2*zcm23*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a2fv)/pop_a3 +r1_a2fv*r1_a2*zcm24*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a2fv)/pop_a4  
    dc_a2fv <- (r3/(r3*5.2 -1))*exp_a2fv - r3*c_a2fv 
    di_a2fv <- mufv*r3*c_a2fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2fv 
    dix_a2fv <- (1-mufv)*r3*c_a2fv-r4*ix_a2fv 
    drx_a2fv <- ix_a2fv*r4 
    ddead_a2fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a2fv
    dsus_a3 <- -r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3)/pop_a1 -r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3)/pop_a2 -r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3)/pop_a3 -r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3)/pop_a4 
    dexp_a3 <- -(r3/(r3*5.2 -1))*exp_a3  + r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3)/pop_a1 +r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3)/pop_a2 +r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3)/pop_a3 +r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3)/pop_a4 
    dci_a3 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a3 - r3*ci_a3 
    dcr_a3 <- alpha*(r3/(r3*5.2 -1))*exp_a3 - cr_a3*(2*r3*r4/(2*r4+r3)) 
    di_a3 <- mu*r3*ci_a3-r11*i_a3 
    dix_a3 <- (1-mu)*r3*ci_a3-r4*ix_a3 
    did_a3 <- cfr_a3*r11*i_a3-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a3 
    dir_a3 <- (1-cfr_a3)*r11*i_a3-r12*ir_a3 
    drx_a3 <- cr_a3*(2*r3*r4/(2*r4+r3))+r4*ix_a3 
    drz_a3 <- r12*ir_a3 
    ddead_a3 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a3 
    dsus_a3v <- -r1_a3v*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3v)/pop_a1 -r1_a3v*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3v)/pop_a2 -r1_a3v*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3v)/pop_a3 -r1_a3v*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3v)/pop_a4 
    dexp_a3v <- -(r3/(r3*5.2 -1))*exp_a3v + r1_a3v*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3v)/pop_a1 +r1_a3v*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3v)/pop_a2 +r1_a3v*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3v)/pop_a3 +r1_a3v*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3v)/pop_a4  
    dc_a3v <- (r3/(r3*5.2 -1))*exp_a3v - r3*c_a3v 
    di_a3v <- muv*r3*c_a3v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3v 
    dix_a3v <- (1-muv)*r3*c_a3v-r4*ix_a3v 
    drx_a3v <- ix_a3v*r4 
    ddead_a3v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3v 
    dsus_a3fv <- -r1_a3fv*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3fv)/pop_a1 -r1_a3fv*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3fv)/pop_a2 -r1_a3fv*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3fv)/pop_a3 -r1_a3fv*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3fv)/pop_a4 
    dexp_a3fv <- -(r3/(r3*5.2 -1))*exp_a3fv + r1_a3fv*r1_a3*zcm31*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a3fv)/pop_a1 +r1_a3fv*r1_a3*zcm32*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a3fv)/pop_a2 +r1_a3fv*r1_a3*zcm33*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a3fv)/pop_a3 +r1_a3fv*r1_a3*zcm34*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a3fv)/pop_a4  
    dc_a3fv <- (r3/(r3*5.2 -1))*exp_a3fv - r3*c_a3fv 
    di_a3fv <- mufv*r3*c_a3fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3fv 
    dix_a3fv <- (1-mufv)*r3*c_a3fv-r4*ix_a3fv 
    drx_a3fv <- ix_a3fv*r4 
    ddead_a3fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a3fv
    dsus_a4 <- -r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4)/pop_a1 -r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4)/pop_a2 -r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4)/pop_a3 -r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4)/pop_a4 
    dexp_a4 <- -(r3/(r3*5.2 -1))*exp_a4  + r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4)/pop_a1 +r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4)/pop_a2 +r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4)/pop_a3 +r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4)/pop_a4 
    dci_a4 <- (1-alpha)*(r3/(r3*5.2 -1))*exp_a4 - r3*ci_a4 
    dcr_a4 <- alpha*(r3/(r3*5.2 -1))*exp_a4 - cr_a4*(2*r3*r4/(2*r4+r3)) 
    di_a4 <- mu*r3*ci_a4-r11*i_a4 
    dix_a4 <- (1-mu)*r3*ci_a4-r4*ix_a4 
    did_a4 <- cfr_a4*r11*i_a4-(r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a4 
    dir_a4 <- (1-cfr_a4)*r11*i_a4-r12*ir_a4 
    drx_a4 <- cr_a4*(2*r3*r4/(2*r4+r3))+r4*ix_a4 
    drz_a4 <- r12*ir_a4 
    ddead_a4 <- (r6*r7*r10/(r7*r10+r6*r10+r6*r7))*id_a4 
    dsus_a4v <- -r1_a4v*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4v)/pop_a1 -r1_a4v*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4v)/pop_a2 -r1_a4v*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4v)/pop_a3 -r1_a4v*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4v)/pop_a4 
    dexp_a4v <- -(r3/(r3*5.2 -1))*exp_a4v + r1_a4v*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4v)/pop_a1 +r1_a4v*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4v)/pop_a2 +r1_a4v*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4v)/pop_a3 +r1_a4v*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4v)/pop_a4  
    dc_a4v <- (r3/(r3*5.2 -1))*exp_a4v - r3*c_a4v 
    di_a4v <- muv*r3*c_a4v-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4v 
    dix_a4v <- (1-muv)*r3*c_a4v-r4*ix_a4v 
    drx_a4v <- ix_a4v*r4 
    ddead_a4v <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4v 
    dsus_a4fv <- -r1_a4fv*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4fv)/pop_a1 -r1_a4fv*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4fv)/pop_a2 -r1_a4fv*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4fv)/pop_a3 -r1_a4fv*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4fv)/pop_a4 
    dexp_a4fv <- -(r3/(r3*5.2 -1))*exp_a4fv + r1_a4fv*r1_a4*zcm41*(ci_a1+cr_a1+omg*i_a1+ki*ix_a1+bet*(id_a1+ir_a1)+c_a1v+omg*i_a1v+ki*ix_a1v+c_a1fv+omg*i_a1fv+ki*ix_a1fv)*(sus_a4fv)/pop_a1 +r1_a4fv*r1_a4*zcm42*(ci_a2+cr_a2+omg*i_a2+ki*ix_a2+bet*(id_a2+ir_a2)+c_a2v+omg*i_a2v+ki*ix_a2v+c_a2fv+omg*i_a2fv+ki*ix_a2fv)*(sus_a4fv)/pop_a2 +r1_a4fv*r1_a4*zcm43*(ci_a3+cr_a3+omg*i_a3+ki*ix_a3+bet*(id_a3+ir_a3)+c_a3v+omg*i_a3v+ki*ix_a3v+c_a3fv+omg*i_a3fv+ki*ix_a3fv)*(sus_a4fv)/pop_a3 +r1_a4fv*r1_a4*zcm44*(ci_a4+cr_a4+omg*i_a4+ki*ix_a4+bet*(id_a4+ir_a4)+c_a4v+omg*i_a4v+ki*ix_a4v+c_a4fv+omg*i_a4fv+ki*ix_a4fv)*(sus_a4fv)/pop_a4  
    dc_a4fv <- (r3/(r3*5.2 -1))*exp_a4fv - r3*c_a4fv 
    di_a4fv <- mufv*r3*c_a4fv-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4fv 
    dix_a4fv <- (1-mufv)*r3*c_a4fv-r4*ix_a4fv 
    drx_a4fv <- ix_a4fv*r4 
    ddead_a4fv <- (r6*r7*r10)/(r7*r10+r6*r10+r6*r7)*i_a4fv
    
    return(list(c(sus_a1=dsus_a1, exp_a1=dexp_a1, ci_a1=dci_a1, cr_a1=dcr_a1, i_a1=di_a1, 
                  ix_a1=dix_a1, id_a1=did_a1, ir_a1=dir_a1, rx_a1=drx_a1, rz_a1=drz_a1, 
                  dead_a1=ddead_a1, sus_a1v=dsus_a1v, exp_a1v=dexp_a1v, c_a1v=dc_a1v, i_a1v=di_a1v, 
                  ix_a1v=dix_a1v, rx_a1v=drx_a1v, dead_a1v=ddead_a1v, sus_a1fv=dsus_a1fv, exp_a1fv=dexp_a1fv, 
                  c_a1fv=dc_a1fv, i_a1fv=di_a1fv, ix_a1fv=dix_a1fv, rx_a1fv=drx_a1fv, dead_a1fv=ddead_a1fv,
                  sus_a2=dsus_a2, exp_a2=dexp_a2, ci_a2=dci_a2, cr_a2=dcr_a2, i_a2=di_a2, 
                  ix_a2=dix_a2, id_a2=did_a2, ir_a2=dir_a2, rx_a2=drx_a2, rz_a2=drz_a2, 
                  dead_a2=ddead_a2, sus_a2v=dsus_a2v, exp_a2v=dexp_a2v, c_a2v=dc_a2v, i_a2v=di_a2v, 
                  ix_a2v=dix_a2v, rx_a2v=drx_a2v, dead_a2v=ddead_a2v, sus_a2fv=dsus_a2fv, exp_a2fv=dexp_a2fv, 
                  c_a2fv=dc_a2fv, i_a2fv=di_a2fv, ix_a2fv=dix_a2fv, rx_a2fv=drx_a2fv, dead_a2fv=ddead_a2fv,
                  sus_a3=dsus_a3, exp_a3=dexp_a3, ci_a3=dci_a3, cr_a3=dcr_a3, i_a3=di_a3, 
                  ix_a3=dix_a3, id_a3=did_a3, ir_a3=dir_a3, rx_a3=drx_a3, rz_a3=drz_a3, 
                  dead_a3=ddead_a3, sus_a3v=dsus_a3v, exp_a3v=dexp_a3v, c_a3v=dc_a3v, i_a3v=di_a3v, 
                  ix_a3v=dix_a3v, rx_a3v=drx_a3v, dead_a3v=ddead_a3v, sus_a3fv=dsus_a3fv, exp_a3fv=dexp_a3fv, 
                  c_a3fv=dc_a3fv, i_a3fv=di_a3fv, ix_a3fv=dix_a3fv, rx_a3fv=drx_a3fv, dead_a3fv=ddead_a3fv,
                  sus_a4=dsus_a4, exp_a4=dexp_a4, ci_a4=dci_a4, cr_a4=dcr_a4, i_a4=di_a4, 
                  ix_a4=dix_a4, id_a4=did_a4, ir_a4=dir_a4, rx_a4=drx_a4, rz_a4=drz_a4, 
                  dead_a4=ddead_a4, sus_a4v=dsus_a4v, exp_a4v=dexp_a4v, c_a4v=dc_a4v, i_a4v=di_a4v, 
                  ix_a4v=dix_a4v, rx_a4v=drx_a4v, dead_a4v=ddead_a4v, sus_a4fv=dsus_a4fv, exp_a4fv=dexp_a4fv, 
                  c_a4fv=dc_a4fv, i_a4fv=di_a4fv, ix_a4fv=dix_a4fv, rx_a4fv=drx_a4fv, dead_a4fv=ddead_a4fv
    )))
  })
}















alpha<-0.4
bet<-0.0001
cfr_a1<-4.9e-5
cfr_a2<-0.0022
cfr_a3<-0.017
cfr_a4<-0.15
ki<-1
mu<-1
mufv<-0.0024
muv<-0.012
omg<-1
r10<-0.14
r11<-0.27
r12<-0.3
# r1_a1<-0.041
r1_a1fv<-0.1
r1_a1v<-0.5  

# r1_a2<-0.041
r1_a2fv<-0.1
r1_a2v<-0.5  

# r1_a3<-0.041
r1_a3fv<-0.1
r1_a3v<-0.5  

# r1_a4<-0.041
r1_a4fv<-0.1
r1_a4v<-0.5  

r3<-0.24
r4<-0.14
r6<-0.9
r7<-1

r9<-2*r3*r4/(2*r4+r3)
rn<-(r6*r7*r10)/(r7*r10+r6*r10+r6*r7)
cfr_m<-mean(c(cfr_a1,cfr_a2,cfr_a3,cfr_a4))

p2_a1=(1-alpha)/r3 + alpha/r9 + (1-alpha)*mu/r11 + (1-alpha)*(1-mu)/r4 + bet*(1-alpha)*mu*cfr_m/r10 +bet*(1-alpha)*mu*(1-cfr_m)/r12;
s_n_nv=1;


# f_a<-c(0.1645, 0.2660, 0.28385, 0.2855)

# f_a<-unlist(demography[input$country == rownames(demography),7:10],use.names = FALSE)



# zcm11<-3.4
# zcm12<-1.7
# zcm13<-1.5
# zcm14<-0.47
# 
# zcm21<-1.1
# zcm22<-3.5
# zcm23<-1.7
# zcm24<-0.54
# 
# zcm31<-0.93
# zcm32<-1.6
# zcm33<-2.4
# zcm34<-0.72
# 
# zcm41<-0.34
# zcm42<-0.62
# zcm43<-0.83
# zcm44<-1.5

# 
# 
# 
# params<-c(alpha=alpha,bet=bet,cfr_a1=cfr_a1,cgr_a2=cfr_a2,cfr_a3=cfr_a3,cfr_a4=cfr_a4,
#           ki=ki,mu=mu,mufv=mufv,muv=muv,omg=omg,r10=r10,r11=r11,r12=r12,r1_a1=r1_a1,
#           r1_a1fv=r1_a1fv,r1_a1v=r1_a1v,r1_a2=r1_a2,r1_a2fv=r1_a2fv,r1_a2v=r1_a2v,r1_a3=r1_a3,
#           r1_a3fv=r1_a3fv,r1_a3v=r1_a3v,r1_a4=r1_a4,r1_a4fv=r1_a4fv,r1_a4v=r1_a4v,r3=r3,r4=r4,r6=r6,
#           r7=r7,zcm11=zcm11,zcm12=zcm12,zcm13=zcm13,zcm14=zcm14,
#           zcm21=zcm21,zcm22=zcm22,zcm23=zcm23,zcm24=zcm24,zcm31=zcm31,zcm32=zcm32,zcm33=zcm33,
#           zcm34=zcm34,zcm41=zcm41,zcm42=zcm42,zcm43=zcm43,zcm44=zcm44)


init_cond_fn<-function(country,imm_a2,imm_a3,imm_a4){

  pop<-unlist(demography[country == rownames(demography),2:5],use.names = FALSE)

  init_cond<-c(sus_a1=pop[1], exp_a1=100, ci_a1=0, cr_a1=0, i_a1=0,
               ix_a1=0, id_a1=0, ir_a1=0, rx_a1=0, rz_a1=0, dead_a1=0,
               sus_a1v=0, exp_a1v=0, c_a1v=0, i_a1v=0, ix_a1v=0, rx_a1v=0, dead_a1v=0,
               sus_a1fv=0, exp_a1fv=0, c_a1fv=0, i_a1fv=0, ix_a1fv=0, rx_a1fv=0, dead_a1fv=0,
               sus_a2=pop[2]*(1-imm_a2), exp_a2=100, ci_a2=0, cr_a2=0, i_a2=0, ix_a2=0, id_a2=0, ir_a2=0, rx_a2=0, rz_a2=0, dead_a2=0,
               sus_a2v=pop[2]*imm_a2, exp_a2v=0, c_a2v=0, i_a2v=0, ix_a2v=0, rx_a2v=0, dead_a2v=0,
               sus_a2fv=0, exp_a2fv=0, c_a2fv=0, i_a2fv=0, ix_a2fv=0, rx_a2fv=0, dead_a2fv=0,
               sus_a3=pop[3]*(1-imm_a3), exp_a3=100, ci_a3=0, cr_a3=0, i_a3=0, ix_a3=0, id_a3=0, ir_a3=0, rx_a3=0, rz_a3=0, dead_a3=0,
               sus_a3v=pop[3]*imm_a3, exp_a3v=0, c_a3v=0, i_a3v=0, ix_a3v=0, rx_a3v=0, dead_a3v=0,
               sus_a3fv=0, exp_a3fv=0, c_a3fv=0, i_a3fv=0, ix_a3fv=0, rx_a3fv=0, dead_a3fv=0,
               sus_a4=pop[4]*(1-imm_a4), exp_a4=100, ci_a4=0, cr_a4=0, i_a4=0, ix_a4=0, id_a4=0, ir_a4=0, rx_a4=0, rz_a4=0, dead_a4=0,
               sus_a4v=pop[4]*imm_a4, exp_a4v=0, c_a4v=0, i_a4v=0, ix_a4v=0, rx_a4v=0, dead_a4v=0,
               sus_a4fv=0, exp_a4fv=0, c_a4fv=0, i_a4fv=0, ix_a4fv=0, rx_a4fv=0, dead_a4fv=0)

  return(init_cond)

}

init_cond_custom<-function(pop,imm_a2,imm_a3,imm_a4){
  
  init_cond<-c(sus_a1=pop[1], exp_a1=100, ci_a1=0, cr_a1=0, i_a1=0,
               ix_a1=0, id_a1=0, ir_a1=0, rx_a1=0, rz_a1=0, dead_a1=0,
               sus_a1v=0, exp_a1v=0, c_a1v=0, i_a1v=0, ix_a1v=0, rx_a1v=0, dead_a1v=0,
               sus_a1fv=0, exp_a1fv=0, c_a1fv=0, i_a1fv=0, ix_a1fv=0, rx_a1fv=0, dead_a1fv=0,
               sus_a2=pop[2]*(1-imm_a2), exp_a2=100, ci_a2=0, cr_a2=0, i_a2=0, ix_a2=0, id_a2=0, ir_a2=0, rx_a2=0, rz_a2=0, dead_a2=0,
               sus_a2v=pop[2]*imm_a2, exp_a2v=0, c_a2v=0, i_a2v=0, ix_a2v=0, rx_a2v=0, dead_a2v=0,
               sus_a2fv=0, exp_a2fv=0, c_a2fv=0, i_a2fv=0, ix_a2fv=0, rx_a2fv=0, dead_a2fv=0,
               sus_a3=pop[3]*(1-imm_a3), exp_a3=100, ci_a3=0, cr_a3=0, i_a3=0, ix_a3=0, id_a3=0, ir_a3=0, rx_a3=0, rz_a3=0, dead_a3=0,
               sus_a3v=pop[3]*imm_a3, exp_a3v=0, c_a3v=0, i_a3v=0, ix_a3v=0, rx_a3v=0, dead_a3v=0,
               sus_a3fv=0, exp_a3fv=0, c_a3fv=0, i_a3fv=0, ix_a3fv=0, rx_a3fv=0, dead_a3fv=0,
               sus_a4=pop[4]*(1-imm_a4), exp_a4=100, ci_a4=0, cr_a4=0, i_a4=0, ix_a4=0, id_a4=0, ir_a4=0, rx_a4=0, rz_a4=0, dead_a4=0,
               sus_a4v=pop[4]*imm_a4, exp_a4v=0, c_a4v=0, i_a4v=0, ix_a4v=0, rx_a4v=0, dead_a4v=0,
               sus_a4fv=0, exp_a4fv=0, c_a4fv=0, i_a4fv=0, ix_a4fv=0, rx_a4fv=0, dead_a4fv=0)
  
  return(init_cond)
  
}


# init_cond_fn<-function(country){
#   
#   pop<-unlist(demography[country == rownames(demography),2:5],use.names = FALSE)
#   
#   init_cond<-c(sus_a1=pop[1], exp_a1=100, ci_a1=0, cr_a1=0, i_a1=0, 
#                ix_a1=0, id_a1=0, ir_a1=0, rx_a1=0, rz_a1=0, dead_a1=0, 
#                sus_a1v=0, exp_a1v=0, c_a1v=0, i_a1v=0, ix_a1v=0, rx_a1v=0, dead_a1v=0, 
#                sus_a1fv=0, exp_a1fv=0, c_a1fv=0, i_a1fv=0, ix_a1fv=0, rx_a1fv=0, dead_a1fv=0, 
#                sus_a2=pop[2], exp_a2=100, ci_a2=0, cr_a2=0, i_a2=0, ix_a2=0, id_a2=0, ir_a2=0, rx_a2=0, rz_a2=0, dead_a2=0, 
#                sus_a2v=pop[2], exp_a2v=0, c_a2v=0, i_a2v=0, ix_a2v=0, rx_a2v=0, dead_a2v=0, 
#                sus_a2fv=0, exp_a2fv=0, c_a2fv=0, i_a2fv=0, ix_a2fv=0, rx_a2fv=0, dead_a2fv=0, 
#                sus_a3=pop[3], exp_a3=100, ci_a3=0, cr_a3=0, i_a3=0, ix_a3=0, id_a3=0, ir_a3=0, rx_a3=0, rz_a3=0, dead_a3=0, 
#                sus_a3v=pop[3], exp_a3v=0, c_a3v=0, i_a3v=0, ix_a3v=0, rx_a3v=0, dead_a3v=0, 
#                sus_a3fv=0, exp_a3fv=0, c_a3fv=0, i_a3fv=0, ix_a3fv=0, rx_a3fv=0, dead_a3fv=0, 
#                sus_a4=pop[4], exp_a4=100, ci_a4=0, cr_a4=0, i_a4=0, ix_a4=0, id_a4=0, ir_a4=0, rx_a4=0, rz_a4=0, dead_a4=0, 
#                sus_a4v=pop[4], exp_a4v=0, c_a4v=0, i_a4v=0, ix_a4v=0, rx_a4v=0, dead_a4v=0, 
#                sus_a4fv=0, exp_a4fv=0, c_a4fv=0, i_a4fv=0, ix_a4fv=0, rx_a4fv=0, dead_a4fv=0)
#   
#   return(init_cond)
#   
# }



posfun <- function(t, y, parms){
  with(as.list(y), {
    y[which(y<0)] <- 0  
    return(y)
  })
}

ode_dynamics <- function(country,init_cond,init_pop,seasonality,age_ratio,cnt_matrix_features,
                         time,R0,cmat_before_measures,cmat_after_measures,vaccination,
                         agreed_vac_percent,gap,delay_measures,apply_measures){
  
  pop_a1<-init_pop[1]
  pop_a2<-init_pop[2]
  pop_a3<-init_pop[3]
  pop_a4<-init_pop[4]
  
  # polymod$contacts<-polymod$contacts[polymod[["contacts"]][["duration_multi"]]>=3]
  
  # bool_reciprocal      <- opt_matrix_features[[1]]  %in% cnt_matrix_features
  # bool_weigh_age       <- opt_matrix_features[[2]]  %in% cnt_matrix_features
  # bool_weigh_dayofweek <- opt_matrix_features[[3]]  %in% cnt_matrix_features
  # bool_age_range       <- opt_matrix_features[[4]]  %in% cnt_matrix_features
  # bool_age_missing     <- opt_matrix_features[[5]]  %in% cnt_matrix_features
  # 
  # baseR_mat <- contact_matrix(survey          = polymod, 
  #                             countries = country,
  #                             age.limits      = c(0, 18, 40,60),
  #                             symmetric       = bool_reciprocal,
  #                             weigh.age       = bool_weigh_age,
  #                             weigh.dayofweek = bool_weigh_dayofweek,
  #                             weight.threshold = weight_threshold,
  #                             estimated.contact.age = ifelse(bool_age_range,'sample','mean'),
  #                             missing.contact.age = ifelse(bool_age_missing,'remove','ignore'),
  #                             return.part.weights = TRUE,
  #                             #return.demography   = TRUE,
  #                             quiet           = TRUE)
  #   
  #  
  # 
  # 
  # baseR_mat<-baseR_mat$matrix
  # print(baseR_mat)

  baseR_mat<-cmat_before_measures
  
  
  
  
  cm_pars<-c('zcm11','zcm12','zcm13','zcm14','zcm21','zcm22','zcm23','zcm24',
             'zcm31','zcm32','zcm33','zcm34','zcm41','zcm42','zcm43','zcm44')
  
  cm_before_measures<-as.vector(t(cmat_before_measures))
  cm_after_measures<-as.vector(t(cmat_after_measures))
  
  wcm<-t(cmat_before_measures)
  f_a<-age_ratio
  
  for (i in 1:4){
    for (j in 1:4){
      baseR_mat[i,j]<-baseR_mat[i,j]*f_a[i]/f_a[j]
    }
  }
  
  eigenvalues = eigen(baseR_mat)$values
  ev=max(eigenvalues)
  
  R1<-R0/((p2_a1*s_n_nv)*ev)
  
  
  
  # print(c(ev,R1))
 
  
  # print(c(zcm11,zcm21,zcm31,zcm41))
  
  
  
  outode<-NULL
  outode<-rbind(outode, c(time=0,init_cond))
  
  timestep<-seq(0,1,0.5)
  vac_start<-30
  
  max_age_2_vac<-pop_a2*(1-agreed_vac_percent[1])
  max_age_3_vac<-pop_a3*(1-agreed_vac_percent[2])
  max_age_4_vac<-pop_a4*(1-agreed_vac_percent[3])
  
  for (i in 1:length(cm_pars)){assign(cm_pars[i],cm_before_measures[i])}
  
  for (simtime in 1:time){
    
    # if (apply_measures=='on'){
    #   
    #   
    #   if (simtime %in% (delay_measures$Delay)){
    #     print(simtime)
    #     working_mat<-as.vector(t(cmat_after_measures[[which(delay_measures$Delay==simtime)]]))
    #     print(working_mat)
    #     for (i in 1:length(cm_pars)){assign(cm_pars[i],working_mat[i])}
    #     print(zcm11)
    #     
    #     
    #   }
    # }
    # else {
    #   for (i in 1:length(cm_pars)){assign(cm_pars[i],cm_before_measures[i])}
    # }
      
      
    
    # else {
    #   for (i in 1:length(cm_pars)){assign(cm_pars[i],cm_before_measures[i])}
    # }

    # for (i in 1:length(cm_pars)){assign(cm_pars[i],cm_before_measures[i])}
    
    if (simtime %in% delay_measures$Delay){
      
    }
    
    r1_a1<-r1_a2<-r1_a3<-r1_a4<-R1*seasonality[simtime]
    
    params<-c(alpha=alpha,bet=bet,cfr_a1=cfr_a1,cgr_a2=cfr_a2,cfr_a3=cfr_a3,cfr_a4=cfr_a4,
              ki=ki,mu=mu,mufv=mufv,muv=muv,omg=omg,r10=r10,r11=r11,r12=r12,r1_a1=r1_a1,
              r1_a1fv=r1_a1fv,r1_a1v=r1_a1v,r1_a2=r1_a2,r1_a2fv=r1_a2fv,r1_a2v=r1_a2v,r1_a3=r1_a3,
              r1_a3fv=r1_a3fv,r1_a3v=r1_a3v,r1_a4=r1_a4,r1_a4fv=r1_a4fv,r1_a4v=r1_a4v,r3=r3,r4=r4,r6=r6,
              r7=r7,zcm11=zcm11,zcm12=zcm12,zcm13=zcm13,zcm14=zcm14,
              zcm21=zcm21,zcm22=zcm22,zcm23=zcm23,zcm24=zcm24,zcm31=zcm31,zcm32=zcm32,zcm33=zcm33,
              zcm34=zcm34,zcm41=zcm41,zcm42=zcm42,zcm43=zcm43,zcm44=zcm44,pop_a1=pop_a1,pop_a2=pop_a2,pop_a3=pop_a3,pop_a4=pop_a4)
    
    
  
    
    
    out <- ode(y =init_cond, times=timestep, covid, params)
    init_cond<-out[length(timestep),2:101]
    out[,1]<-out[,1]+simtime-1
    outode<-rbind(outode,out[length(timestep),])
    
  #   # 1st vaccination
  #   
   

    if (simtime>vac_start){

      # o_vac<-400000
      # y_vac<-100000
      # yy_vac<-100000

      o_vac<-vaccination[3]
      y_vac<-vaccination[2]
      yy_vac<-vaccination[1]

      d_vac<-o_vac+y_vac

      # 4th age group
      
      if (init_cond[76]>max_age_4_vac){
        init_cond[76]<-init_cond[76]-o_vac;
        init_cond[87]<-init_cond[87]+o_vac;
        o_vac<-0;

      }
      
    

      # 3rd age group

      if (init_cond[51]>max_age_3_vac){
        init_cond[51]<-init_cond[51]-(y_vac+o_vac)
        init_cond[62]<-init_cond[62]+(y_vac+o_vac)
        y_vac<-0;
        o_vac<-0;
        d_vac<-0;
        if (init_cond[51]<max_age_3_vac){
          cnt<-max_age_3_vac-init_cond[51]
          init_cond[51]<-max_age_3_vac
          init_cond[62]<-init_cond[62]-cnt
          init_cond[26]<-init_cond[26]-cnt
          init_cond[37]<-init_cond[37]+cnt
        }

      }

      if (init_cond[26]>max_age_2_vac){
        init_cond[26]<-init_cond[26]-(yy_vac+d_vac)
        init_cond[37]<-init_cond[37]+yy_vac+d_vac
        d_vac<-0
        if (init_cond[26]<max_age_2_vac){
          init_cond[37]<-init_cond[37]-(max_age_2_vac-init_cond[26])
          init_cond[26]<-max_age_2_vac

        }
      }

      ## 2nd vaccine

      if (simtime>(vac_start+gap)){
        # d_fvac<-500000
        # o_fvac<-300000
        # m_fvac<-175000
        # y_fvac<-25000


        o_fvac<-vaccination[6]
        m_fvac<-vaccination[5]
        y_fvac<-vaccination[4]
        d_fvac<-y_fvac+m_fvac+o_fvac

        if (init_cond[87]>10){
          init_cond[87]<-init_cond[87]-o_fvac
          init_cond[94]<-init_cond[94]+o_fvac
          d_fvac<-d_fvac-o_fvac

          if (init_cond[87]<0){
            
            init_cond[94]<-init_cond[94]+init_cond[87] # because its negative
            init_cond[62]<-init_cond[62]+init_cond[87]
            init_cond[69]<-init_cond[69]-init_cond[87]
            init_cond[87]<-0
          }
        }

        if (init_cond[62]>10 && init_cond[87]==0){
          init_cond[62]<-init_cond[62]-o_fvac-m_fvac
          init_cond[69]<-init_cond[69]+o_fvac+m_fvac
          d_fvac<-d_fvac-o_fvac-m_fvac

          if (init_cond[62]<0){
            init_cond[69]<-init_cond[69]+init_cond[62] # because its negative
            init_cond[37]<-init_cond[37]+init_cond[62]
            init_cond[44]<-init_cond[44]-init_cond[62]
            init_cond[62]<-0

          }
        }
        else if (init_cond[62]>10){
            init_cond[62]<-init_cond[62]-m_fvac
            init_cond[69]<-init_cond[69]+m_fvac
            d_fvac<-d_fvac-m_fvac
            if (init_cond[62]<0){
              init_cond[69]<-init_cond[69]+init_cond[62] # because its negative
              init_cond[37]<-init_cond[37]+init_cond[62]
              init_cond[44]<-init_cond[44]-init_cond[62]
              init_cond[62]<-0
            }
        }



        if (init_cond[37]>10 && init_cond[62]==0){
          init_cond[37]<-init_cond[37]-d_fvac
          init_cond[44]<-init_cond[44]+d_fvac
          d_fvac<-d_fvac-d_fvac
          if (init_cond[37]<0){
            init_cond[44]<-init_cond[44]+init_cond[37] # because its negative
            init_cond[37]<-0

          }
        }
        else if (init_cond[37]>10){
          init_cond[37]<-init_cond[37]-y_fvac
          init_cond[44]<-init_cond[44]+y_fvac
          d_fvac<-d_fvac-y_fvac

          if (init_cond[37]<0){
            init_cond[44]<-init_cond[44]+init_cond[37] # because its negative
            init_cond[37]<-0
          }
        }



      }


    }


  }
  
  # print(rowSums(outode[,2:(ncol(outode))]))
  
  ode_a1_inf<-diff(rowSums(outode[,c('i_a1','id_a1','ir_a1','rz_a1','dead_a1','i_a1v','ix_a1v','rx_a1v','dead_a1v','i_a1fv','ix_a1fv','rx_a1fv','dead_a1fv')]))
  ode_a2_inf<-diff(rowSums(outode[,c('i_a2','id_a2','ir_a2','rz_a2','dead_a2','i_a2v','ix_a2v','rx_a2v','dead_a2v','i_a2fv','ix_a2fv','rx_a2fv','dead_a2fv')]))
  ode_a3_inf<-diff(rowSums(outode[,c('i_a3','id_a3','ir_a3','rz_a3','dead_a3','i_a3v','ix_a3v','rx_a3v','dead_a3v','i_a3fv','ix_a3fv','rx_a3fv','dead_a3fv')]))
  ode_a4_inf<-diff(rowSums(outode[,c('i_a4','id_a4','ir_a4','rz_a4','dead_a4','i_a4v','ix_a4v','rx_a4v','dead_a4v','i_a4fv','ix_a4fv','rx_a4fv','dead_a4fv')]))
  tot_inf<-ode_a1_inf+ode_a2_inf+ode_a3_inf+ode_a4_inf
  
  
  ode_a1_dead<-diff(rowSums(outode[,c('dead_a1','dead_a1v','dead_a1fv')]))
  ode_a2_dead<-diff(rowSums(outode[,c('dead_a2','dead_a2v','dead_a2fv')]))
  ode_a3_dead<-diff(rowSums(outode[,c('dead_a3','dead_a3v','dead_a3fv')]))
  ode_a4_dead<-diff(rowSums(outode[,c('dead_a4','dead_a4v','dead_a4fv')]))
  tot_dead<-ode_a1_dead+ode_a2_dead+ode_a3_dead+ode_a4_dead
  
  ode_time<-outode[-c(1),'time']
  
  dy_data=data.frame(time=ode_time,a1_inf=ode_a1_inf, a2_inf=ode_a2_inf, 
                     a3_inf=ode_a3_inf, a4_inf=ode_a4_inf, tot_inf=tot_inf,
                     a1_dead=ode_a1_dead, a2_dead=ode_a2_dead,
                     a3_dead=ode_a3_dead, a4_dead=ode_a4_dead, tot_dead=tot_dead)
  return(dy_data)
  
  
}


plot_tot_inf<-function(data){

  colnames(data)<-c('Days','0-18 daily incidences','18-40 daily incidences',
                    '40-60 daily incidences','60+ daily incidences', 'Incidences',
                    '0-18 daily deaths','18-40 daily deaths','40-60 daily deaths','60+ daily deaths','Total daily deaths','Day')
  # data$Day<-seq(as.Date(start_day)+1, as.Date(end_day), "days")
  
  new_data<-data[,c("Day","Incidences")]
  
   p1<-plot_ly(data = new_data, x = ~Days, y = ~Incidences,mode = 'lines',line=list(color = 'rgb(0,100,0)', width = 4))%>%
               layout(xaxis = list(title = list(text ='<b>Days</b>', font = list(size=14,color='black'))),
               yaxis = list(title = list(text='Daily total incidences',font=list(size=14,color='black'))))

   p1<-ggplot(data, mapping=aes(x=Days))+
     geom_line(aes(y=Incidences),color='darkgreen',size=1.5)+
     scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Total Daily Incidences")+
     theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))

  # p1 <- figure(width=600, height=450) %>% ly_points(Day, Incidences, data = new_data,
  #             color = 'white',
  #             hover = list(Day, Incidences)) %>% ly_lines(Day, Incidences, data = new_data,
  #                                                           color = 'darkgreen',width = 3) %>%
  #   theme_axis(c("y"),axis_label_text_color = "black", axis_label_text_font_size = "12pt",
  #              major_label_text_font_size = "12pt",
  #              major_label_text_color = "black"
  #              )%>%
  #   theme_axis(c("x"),major_label_orientation = 45,axis_label_text_alpha = 0,
  #              major_label_text_font_size = "12pt",
  #              major_label_text_color = "black"
  #              )
  #   
  p1
  
  # p <- figure() %>%ly_points(Sepal.Length, Sepal.Width, data = iris,
  #                            color = Species, glyph = Species,
  #                            hover = list(Sepal.Length, Sepal.Width)) %>%ly_lines(Sepal.Length, Sepal.Width, data = iris,
  #                                                                                  color = Species)
  # 
  # p
  
}
  
plot_tot_dead<-function(data){
  colnames(data)<-c('Days','0-18 daily incidences','18-40 daily incidences',
                    '40-60 daily incidences','60+ daily incidences', 'Incidences',
                    '0-18 daily deaths','18-40 daily deaths','40-60 daily deaths','60+ daily deaths','Deaths')
  new_data<-data[,c("Days","Deaths")]
  
  # p2<-plot_ly(data = new_data, x = ~Days, y = ~Deaths,mode = 'lines',line=list(color = 'rgb(139,0,0)', width = 4))
  
  
  
  # p2<-ggplot(data, mapping=aes(x=Days))+
  #   geom_line(aes(y=Deaths),color='darkred',size=1.5)+scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Total Daily Deaths")+
  #   theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  # 
  # p1<-ggplotly(p1, tooltip=c("x", "y"))
  
  
  
  # ggarrange(p1,p2,ncol=2)
  
  p1 <- figure(width=600, height=450) %>% ly_points(Days, Deaths, data = new_data,
                                                    color = 'white',
                                                    hover = list(Days, Deaths)) %>% ly_lines(Days, Deaths, data = new_data,
                                                                                                 color = 'darkred',width = 3) %>%
    theme_axis(c("x", "y"),axis_label_text_color = "black", axis_label_text_font_size = "12pt",
               major_label_text_font_size = "12pt",
               major_label_text_color = "black",
    )
  p1
  
}

plot_age_inf<-function(data){
  
  # colnames(data)<-c('Days','0-18 daily incidences','18-40 daily incidences',
  #                   '40-60 daily incidences','60+ daily incidences', 'Incidences',
  #                   '0-18 daily deaths','18-40 daily deaths','40-60 daily deaths','60+ daily deaths','Deaths')
  # 
  p1<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a1_inf),color='darkgreen',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Daily Incidences (0-18)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))

  p2<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a2_inf),color='darkgreen',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Daily Incidences (18-40)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))

  p3<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a3_inf),color='darkgreen',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Daily Incidences (40-60)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))

  p4<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a4_inf),color='darkgreen',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Daily Incidences (60+)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))

  ggarrange(p1,p2,p3,p4,ncol=4)
  
  # p1 <- figure(width=600, height=450) %>% ly_points(Days, '0-18 daily incidences', data = data,
  #                                                   color = 'white',
  #                                                   hover = list(Days, '0-18)) %>% ly_lines(Days, '0-18 daily incidences', data = data,
  #                                                                                                color = 'darkred',width = 3)
  # p1
}

plot_age_inf_log<-function(data){
  
  # colnames(data)<-c('Days','0-18 daily incidences','18-40 daily incidences',
  #                   '40-60 daily incidences','60+ daily incidences', 'Incidences',
  #                   '0-18 daily deaths','18-40 daily deaths','40-60 daily deaths','60+ daily deaths','Deaths')
  # 
  p1<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a1_inf),color='darkgreen',size=1.5)+
    scale_y_continuous(trans='log10') +xlab("Days")+ylab("Daily Incidences (0-18)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  p2<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a2_inf),color='darkgreen',size=1.5)+
    scale_y_continuous(trans='log10') +xlab("Days")+ylab("Daily Incidences (18-40)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  p3<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a3_inf),color='darkgreen',size=1.5)+
    scale_y_continuous(trans='log10') +xlab("Days")+ylab("Daily Incidences (40-60)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  p4<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a4_inf),color='darkgreen',size=1.5)+
    scale_y_continuous(trans='log10') +xlab("Days")+ylab("Daily Incidences (60+)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  ggarrange(p1,p2,p3,p4,ncol=4)
  
  # p1 <- figure(width=600, height=450) %>% ly_points(Days, '0-18 daily incidences', data = data,
  #                                                   color = 'white',
  #                                                   hover = list(Days, '0-18)) %>% ly_lines(Days, '0-18 daily incidences', data = data,
  #                                                                                                color = 'darkred',width = 3)
  # p1
}

plot_age_inf_plotly<-function(data){
  # p <- plot_ly(data, type = 'scatter', mode = 'lines',line = list(width = 4))%>% 
  #   layout( xaxis = list(title = "Days"),
  #              yaxis = list (title = "Daily incidences")) %>%
  #   add_trace(x = ~time, y = ~a1_inf, name = '0-18', hoverinfo='text', text=~paste('</br>Days: ',time,'</br>0-18 Incidences: ',round(a1_inf))) %>%
  #   add_trace(x = ~time, y = ~a2_inf, name = '18-40', hoverinfo='text', text=~paste('</br>Days: ',time,'</br>18-40 Incidences: ',round(a2_inf)))%>%
  #   add_trace(x = ~time, y = ~a3_inf, name = '40-60', hoverinfo='text', text=~paste('</br>Days: ',time,'</br>40-60 Incidences: ',round(a3_inf)))%>%
  #   add_trace(x = ~time, y = ~a4_inf, name = '60+', hoverinfo='text', text=~paste('</br>Days: ',time,'</br>60+ Incidences: ',round(a4_inf)))
  # 
  # ay <- list(
  #   tickfont = list(color = 'rgb(22, 96, 167)'),
  #   overlaying = "y",
  #   side = "right",
  #   title = "Seasonality")
  # 
  # p <- p %>% add_trace(x = ~time, y = ~seasonality, name = 'Seasonality', yaxis = "y2",type = 'scatter', 
  #                      mode = 'lines', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'),
  #                                                  hoverinfo='text', text=~paste('</br>Days: ',time,'</br>Seasonality: ',seasonality))
  # 
  # p <- p %>% layout(
  #   title = "",yaxis2 = ay,legend = list(orientation = 'v',x = 100, y = 1),
  #   updatemenus = list(
  #     list(
  #       type = "buttons",
  #       direction = "right",
  #         xanchor = 'center',
  #         yanchor = "top",
  #         # pad = list('r'= 0, 't'= 10, 'b' = 0),
  #         x = 0.5,
  #         y = 1.27,
  #         buttons = list(
  #           list(method = "update",args = list(list("visible", c(T,T,T,T,T)),list(yaxis = list(type = "linear",title = "Daily incidences"))),
  #                label = "Linear"),
  #           list(method = "update",args = list(list("visible", c(T,T,T,T,T)),list(yaxis = list(type = "log",title = "Daily incidences"))),
  #                label = "Log")
  #         )
  #       )
  #   )
  # )
  # p
  # 
}


plot_age_dead<-function(data){
  
  p1<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a1_dead),color='darkred',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),10))) } ) +xlab("Days")+ylab("Daily Deaths (0-18)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  p2<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a2_dead),color='darkred',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Daily Deaths (18-40)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  p3<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a3_dead),color='darkred',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Daily Deaths (40-60)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  p4<-ggplot(data, mapping=aes(x=time))+
    geom_line(aes(y=a4_dead),color='darkred',size=1.5)+
    scale_y_sqrt(breaks = function(x) { pretty(c(min(x)/5,seq(min(x),max(x),100))) } ) +xlab("Days")+ylab("Daily Deaths (60+)")+
    theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"))
  
  ggarrange(p1,p2,p3,p4,ncol=4)
}

download_ode_data<-function(data){
  colnames(data)<-c('Time','0-18 daily incidences','18-40 daily incidences',
                    '40-60 daily incidences','60+ daily incidences', 'Total daily incidences',
                    '0-18 daily deaths','18-40 daily deaths','40-60 daily deaths','60+ daily deaths','Total daily deaths')
  return(data)
}

# function(time){
#   for
#   out<-ode(y =init_cond, times=seq(0, 1, 0.1), covid, params)}