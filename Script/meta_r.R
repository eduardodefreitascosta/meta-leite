

#Reset the envoronment
rm(list = ls())

#Packages to be used
packages<-c("readxl","here","tidyverse","ggplot2","meta","metafor","knitr")


# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))



meta<-read.csv(here("Data","meta.csv"),sep=";")

##meta subgrupos###

sub1<-subset(meta,meta$ATB=="Pen")
sub2<-subset(meta,meta$ATB=="Amp")
sub3<-subset(meta,meta$ATB=="Cefa")
sub4<-subset(meta,meta$ATB=="Oxa")

teste1<-paste(sub1$Estudo," ","et al.", "(",sub1$ano,")", sep="")
teste2<-paste(sub2$Estudo," ","et al.", "(",sub2$ano,")", sep="")
teste3<-paste(sub3$Estudo," ","et al.", "(",sub3$ano,")", sep="")
teste4<-paste(sub4$Estudo," ","et al.", "(",sub4$ano,")", sep="")


##facil metaprop##

meta_sub1<-metaprop(sub1$Res,sub1$Total,teste1,sm="plogit",method.tau= "DL", incr=0.5)

png(file=here("Figures","pen.png"),
    width = 500, height = 225, units='mm', res = 300)
forest(meta_sub1,slab=teste1, xlim=c(0,1),xlab="Prevalence",comb.fixed=F, print.I2 =T,hetlab="Heterogeneity : ",print.pval.Q=F, leftlabs = c("Study, year", "Positive", "Total"),rightlabs = c("Prevalence [95% CI]  Weight"),margin = c(0,0,0, 0))
dev.off()

meta_sub2<-metaprop(sub2$Res,sub2$Total,teste2,sm="plogit",method.tau= "DL", incr=0.5)

png(file=here("Figures","amp.png"), width = 500, height = 225, units='mm', res = 300)
forest(meta_sub2,slab=teste2, xlim=c(0,1),xlab="Prevalence",comb.fixed=F, print.I2 =T,hetlab="Heterogeneity : ",print.pval.Q=F, leftlabs = c("Study, year", "Positive", "Total"),rightlabs = c("Prevalence [95% CI]  Weight"),margin = c(6,12,4, 10))
dev.off()


meta_sub3<-metaprop(sub3$Res,sub3$Total,teste3,sm="plogit",method.tau= "DL", incr=0.5)

png(file=here("Figures","cefa.png"), 
    width = 500, height = 225, units='mm', res = 300)
forest(meta_sub3,slab=teste3, xlim=c(0,0.1),xlab="Prevalence",comb.fixed=F, print.I2 =T,hetlab="Heterogeneity : ",print.pval.Q=F,  leftlabs = c("Study, year", "Positive", "Total"),rightlabs = c("Prevalence [95% CI]  Weight"),margin = c(6,12,4, 10))
dev.off()




meta_sub4<-metaprop(sub4$Res,sub4$Total,teste4,sm="plogit",method.tau= "DL", incr=0.5)

png(file=here("Figures","oxa.png"),
    width = 500, height = 225, units='mm', res = 300)
forest(meta_sub4,slab=teste4, xlim=c(0,0.2),xlab="Prevalence",comb.fixed=F, print.I2 =T,hetlab="Heterogeneity : ",print.pval.Q=F,  leftlabs = c("Study, year", "Positive", "Total"),rightlabs = c("Prevalence [95% CI]  Weight"),margin = c(6,12,4, 10))
dev.off()




##regres?o_penicilina##
res1_ano<-rma(xi=sub1$Res,mi=sub1$Nao_Resistentes,mods=~sub1$ano,measure="PLO", method="DL")
res1_metodo<-rma(xi=sub1$Res,mi=sub1$Nao_Resistentes,mods=~factor(sub1$Metodo),measure="PLO", method="DL")
res1_Mastite<-rma(xi=sub1$Res,mi=sub1$Nao_Resistentes,mods=~factor(sub1$Mastite),measure="PLO", method="DL")
res1_alloc<-rma(xi=sub1$Res,mi=sub1$Nao_Resistentes,mods=~factor(sub1$set),measure="PLO", method="DL")
res1_pais<-rma(xi=sub1$Res,mi=sub1$Nao_Resistentes,mods=~factor(sub1$Pais),measure="PLO", method="DL")

res1<-rma(xi=sub1$Res,mi=sub1$Nao_Resistentes,mods=~factor(sub1$set)+factor(sub1$Metodo),measure="PLO", method="DL")

##regress?o_ampicilina##
res2_ano<-rma(xi=sub2$Res,mi=sub2$Nao_Resistentes,mods=~sub2$ano,measure="PLO", method="DL")
res2_metodo<-rma(xi=sub2$Res,mi=sub2$Nao_Resistentes,mods=~factor(sub2$Metodo),measure="PLO", method="DL")
res2_Mastite<-rma(xi=sub2$Res,mi=sub2$Nao_Resistentes,mods=~factor(sub2$Mastite),measure="PLO", method="DL")
res2_alloc<-rma(xi=sub2$Res,mi=sub2$Nao_Resistentes,mods=~factor(sub2$set),measure="PLO", method="DL")
res2_pais<-rma(xi=sub2$Res,mi=sub2$Nao_Resistentes,mods=~factor(sub2$Pais),measure="PLO", method="DL")

res2<-rma(xi=sub2$Res,mi=sub2$Nao_Resistentes,mods=~factor(sub2$Metodo)+factor(sub2$Pais),measure="PLO", method="DL")

##regress?o_cefa##
res3_ano<-rma(xi=sub3$Res,mi=sub3$Nao_Resistentes,mods=~sub3$ano,measure="PLO", method="DL")
res3_metodo<-rma(xi=sub3$Res,mi=sub3$Nao_Resistentes,mods=~factor(sub3$Metodo),measure="PLO", method="DL")
res3_Mastite<-rma(xi=sub3$Res,mi=sub3$Nao_Resistentes,mods=~factor(sub3$Mastite),measure="PLO", method="DL")
res3_alloc<-rma(xi=sub3$Res,mi=sub3$Nao_Resistentes,mods=~factor(sub3$set),measure="PLO", method="DL")
res3_pais<-rma(xi=sub3$Res,mi=sub3$Nao_Resistentes,mods=~factor(sub3$Pais),measure="PLO", method="DL")

res2<-rma(xi=sub2$Res,mi=sub2$Nao_Resistentes,mods=~factor(sub2$Metodo)+factor(sub2$Pais),measure="PLO", method="DL")

##regress?o_oxa##
res4_ano<-rma(xi=sub4$Res,mi=sub4$Nao_Resistentes,mods=~sub4$ano,measure="PLO", method="DL")
res4_metodo<-rma(xi=sub4$Res,mi=sub4$Nao_Resistentes,mods=~factor(sub4$Metodo),measure="PLO", method="DL")
res4_Mastite<-rma(xi=sub4$Res,mi=sub4$Nao_Resistentes,mods=~factor(sub4$Mastite),measure="PLO", method="DL")
res4_alloc<-rma(xi=sub4$Res,mi=sub4$Nao_Resistentes,mods=~factor(sub4$set),measure="PLO", method="DL")
res4_pais<-rma(xi=sub4$Res,mi=sub4$Nao_Resistentes,mods=~factor(sub4$Pais),measure="PLO", method="DL")

res4<-rma(xi=sub4$Res,mi=sub4$Nao_Resistentes,mods=~factor(sub4$Mastite)+factor(sub4$set),measure="PLO", method="DL",drop00=T)

