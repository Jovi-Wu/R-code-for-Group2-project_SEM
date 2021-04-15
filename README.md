# R-code-for-Group2-project_SEM
This is a sharing of R code for Group 2 project in Quantitative method 
setwd("/Users/wuqiaofei/Desktop/Rdata")
group2 <- read.csv("/users/wuqiaofei/Desktop/Rdata/group2.csv")
head(group2)
group2$final_risk_perception3
library(car)
group2$recode_final_risk_perception3 <- recode(group2$final_risk_perception3,"1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1")
names(group2)
names(group2)[2:12] <- c("ex","ol1","ol2","ol3","t1","t2","t3","rs1","rs2","rs3","rrs3")   #rename the columns


#calculate Cronbach's alpha for reliablity
library(psy)
cronbach(group2[,3:5]) #perc_overload   0.9626334
cronbach(group2[,6:8]) #trust     0.7863974
cronbach(group2[,c(9,10,12)]) #risk perception 0.9604079


#generate new variables 
group2$ol <- (group2$ol1+group2$ol2+group2$ol3)/3   #inforation overload
group2$ts <- (group2$t1+group2$t2+group2$t3)/3      #trust
group2$rp <- (group2$rs1+group2$rs2+group2$rrs3)/3  #risk perception
round(apply(group2,2,mean,na.rm = T),2) #  ex  4.41 ol 3.96 ts 2.75 rp  4.11
round(apply(group2,2,sd,na.rm = T),2)  #  ex  1.12  ol 1.04 ts 0.42 rp  1.18
library(psych)    # or one step for mean and sd for every variable
describe(group2)    


#statistical analysis
#analysis of mediation effect of information overload
library(lavaan)
HS.model <- ' #measurement model
              finalex =~ ex
              finalol =~ ol1 + ol2 + ol3
              finalts =~ t1 + t2 + t3
              finalrs =~ rs1 + rs2 + rrs3
              
              #regression
              #direct effect
              finalrs ~c*finalex
              #mediator
              finalol ~a*finalex
              finalrs ~b*finalol
              # indirect effect (a*b)
              ab := a*b
              # total effect
              total := c + (a*b)
                '
fit <- sem(HS.model, data = group2,test= "boot")
summary(fit, fit.measure = TRUE,standardized =TRUE)
parameterEstimates(fit)
library(semPlot)
semPaths(fit,whatLabels = "est", edge.label.cex = 1, layout ="tree")

# analysis of moderation effect of trust in news on social media
#mean-centering
group2$ex.c <- scale(group2$ex,center = TRUE, scale = FALSE)[,]   #Centralization
group2$ol.c <- scale(group2$ol,center = TRUE, scale = FALSE)[,]   #Centralization
group2$ts.c <- scale(group2$ts,center = TRUE, scale = FALSE)[,]   #Centralization

#why mean-centering?
cor(group2$ex,group2$ts*group2$ex)
cor(group2$ts,group2$ts*group2$ex)
cor(group2$ex.c,group2$ts.c*group2$ex.c)
cor(group2$ts.c,group2$ts.c*group2$ex.c)

#fit model
fitmod1 <- lm(ol~ ex.c*ts.c,data = group2)
summary(fitmod1)
fitmod2 <- lm(rp~ ol.c*ts.c,data = group2)
summary(fitmod2)
library(rockchalk)
ps2  <- plotSlopes(fitmod2, plotx="ol.c", modx="ts.c", xlab = "Perceived news overload", ylab = "Risk perception", 
                   plotPoints = F,plotxRange = c(1,3),ylim = c(0,5),legendArgs = list(title = "Trust in news on SM"),
                   interval = "confidence", opacity = 100, col = c("red","green","blue"),
                   modxVals = "std.dev")
