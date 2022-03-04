library(ggvegan)

data<-read.csv("shared ARGs.csv",row.names=1)
env<-read.csv("MGEs&Class.csv",row.names=1)
env.data.log <- log1p(env)
env <- na.omit(env.data.log)
data.hell<- decostand(data, "hellinger")
sel <- decorana(data.hell)
data.tab.0 <- rda(data.hell ~ 1, env)
data.tab.1<- rda(data.hell ~ ., env)
vif.cca(data.tab.1)
#all variables less than 10
mod.u <- step(data.tab.0, scope = formula(arg.tab.1), test = "perm")
mod.d <- step(data.tab.0, scope = (list(lower = formula(arg.tab.0), upper = formula(arg.tab.1))))
arg.rda.f <- rda(data.hell ~ Alphaproteobacteria+Bacilli+Bacteroidia+Betaproteobacteria+Clostridia+Deltaproteobacteria+Epsilonproteobacteria
                 +Erysipelotrichia+Fusobacteriia+Gammaproteobacteria+Viruses_noname+plasmid+
                   IS621+insertion_element_IS91+istA2+Tn916, env)
anova(arg.rda.f)
anova(arg.rda.f, by = "term")
anova(arg.rda.f, by = "axis")
autoplot(arg.rda.f, arrows = TRUE,axes = c(1, 2), geom =  c("point", "text"), 
         layers = c("biplot", "centroids"), legend.position = "right", title = "db-RDA")+
  theme_bw()+theme(panel.grid=element_blank())
