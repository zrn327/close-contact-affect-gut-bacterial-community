library(vegan)
explain <- function(a,b){
  a/b
}
data<-read.csv("shared ARGs.csv",row.names=1)
env<-read.csv("retained MGEs&Class.csv",row.names=1)
env.data.log <- log1p(env)
env <- na.omit(env.data.log)
p1 <- summary(rda(data,env))
paste("explained:", explain(p1[["constr.chi"]],p1[["tot.chi"]]))
paste("unexplained:", explain(p1[["constr.chi"]],p1[["tot.chi"]]))
p2 <- summary(rda(data, env[,1:7],env[,-(1:7)]))
paste("bacterial community explained:", explain(p4[["constr.chi"]],p4[["tot.chi"]]))
p3 <- summary(rda(data, env[,8:17],env[,-(8:17)]))
paste("MGE explained:", explain(p3[["constr.chi"]],p3[["tot.chi"]]))
paste("bacterial community & MGE explained:",explain(p1[["constr.chi"]],p1[["tot.chi"]])-
        explain(p3[["constr.chi"]],p3[["tot.chi"]])-
        explain(p4[["constr.chi"]],p4[["tot.chi"]]))