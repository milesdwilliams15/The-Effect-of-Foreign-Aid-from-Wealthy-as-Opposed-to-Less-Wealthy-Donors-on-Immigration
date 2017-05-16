


path<-file.path(...,"data.csv")
immig<-read.csv(path)

## Estimate an OLS model
OLS1<-lm(logrInflowLead~logdAid+logrInflow+logdrDistance+drColony+drLanguage+
          dRestrict+logdGDPcap+dUnemployment+logdPopulation+
          rConflict+rDemo+as.factor(Donor)+(as.factor(year)*as.factor(Recipient)),
        data=immig)
immig$logdAid2<-immig$logdAid^2
OLS2<-lm(logrInflowLead~logdAid+logdAid2+logrInflow+logdrDistance+drColony+drLanguage+
      dRestrict+logdGDPcap+dUnemployment+logdPopulation+
           rConflict+rDemo+as.factor(Donor)+(as.factor(year)*as.factor(Recipient)),
         data=immig)
library(dplyr)
immig2<-filter(immig,Donor=="US"|Donor=="JPN"|Donor=="GRM"|Donor=="FRN"|Donor=="UK"|Donor=="ITY")
OLS3<-lm(logrInflowLead~logdAid+logdAid2+logrInflow+logdrDistance+drColony+drLanguage+
           dRestrict+logdGDPcap+dUnemployment+logdPopulation+
           rConflict+rDemo+as.factor(Donor)+(as.factor(year)*as.factor(Recipient)),
         data=immig2)
immig3<-filter(immig,Donor=="NZD"|Donor=="SWZ"|Donor=="NWY"|Donor=="FLD"|Donor=="DMK")
OLS4<-lm(logrInflowLead~logdAid+logdAid2+logrInflow+logdrDistance+drColony+drLanguage+
           dRestrict+logdGDPcap+dUnemployment+logdPopulation+
           rConflict+rDemo+as.factor(Donor)+(as.factor(year)*as.factor(Recipient)),
         data=immig3)
immig4<-filter(immig,Donor=="NZD"|Donor=="SWZ"|Donor=="NWY"|Donor=="FLD"|Donor=="DMK")
OLS4<-lm(logrInflowLead~logdAid+logdAid2+logrInflow+logdrDistance+drColony+drLanguage+
           dRestrict+logdGDPcap+dUnemployment+logdPopulation+
           rConflict+rDemo+as.factor(Donor)+(as.factor(year)*as.factor(Recipient)),
         data=immig3)
# Get clustered standard errors
library(lmtest)
# OLS1
G <- length(unique(immig$Dyad))
N <- length(immig$Dyad)
dfa <- (G/(G - 1)) * (N - 1)/OLS1$df.residual
dyad_c_vcov1 <- dfa * vcovHC(OLS1, type = "HC0", cluster = "Dyad", adjust = T)
OLSc1 <- coeftest(OLS1, vcov = dyad_c_vcov1)

#OLS2
G <- length(unique(immig$Dyad))
N <- length(immig$Dyad)
dfa <- (G/(G - 1)) * (N - 1)/OLS2$df.residual
dyad_c_vcov2 <- dfa * vcovHC(OLS2, type = "HC0", cluster = "Dyad", adjust = T)
OLSc2 <- coeftest(OLS2, vcov = dyad_c_vcov2)

#OLS3
G <- length(unique(immig2$Dyad))
N <- length(immig2$Dyad)
dfa <- (G/(G - 1)) * (N - 1)/OLS3$df.residual
dyad_c_vcov3 <- dfa * vcovHC(OLS3, type = "HC0", cluster = "Dyad", adjust = T)
OLSc3 <- coeftest(OLS3, vcov = dyad_c_vcov3)

#OLS4
G <- length(unique(immig3$Dyad))
N <- length(immig3$Dyad)
dfa <- (G/(G - 1)) * (N - 1)/OLS4$df.residual
dyad_c_vcov4 <- dfa * vcovHC(OLS4, type = "HC0", cluster = "Dyad", adjust = T)
OLSc4 <- coeftest(OLS4, vcov = dyad_c_vcov4)

# Create Tables
write.csv(OLScl[1:12,],"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/OLS1.csv")
write.csv(OLSc2[1:13,],"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/OLS2.csv")
write.csv(OLSc3[1:13,],"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/OLS3.csv")
write.csv(OLSc4[1:12,],"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/OLS4.csv")

x<-seq(0,8.151,0.01)
x2<-x^2
yOls<-coef(OLS2)[2]*x + coef(OLS2)[3]*x2
yTob<-coeftest(tob2)[3,1]*x + coeftest(tob2)[4,1]*x2

windows()
par(bty="l",family="serif")
plot(x,yOls,type="n",xlab="Natural Log of Bilateral Aid at Year t-1",
     ylab="Natural Log of Bilateral Migrant Inflows at Year t",lwd=1.75,col="grey35")
abline(h=seq(0,0.3,.05),col="lightgrey")
abline(v=seq(0,8,2),col="lightgrey")
lines(x,yTob,lwd=2,col="grey50")
lines(x,yOls,lwd=2,col="grey35")
legend("right",bg="white",legend=c("OLS","Tobit"),col=c("grey35","grey50"),lwd=c(2,2),lty=c(1,1),
       title="Model")

yW<-coef(OLS3)[2]*x + coef(OLS3)[3]*x2
yWu<-(coef(OLS3)[2]+OLSc3[2,2])*x + (coef(OLS3)[3]+OLSc3[3,2])*x2
yWl<-(coef(OLS3)[2]-OLSc3[2,2])*x + (coef(OLS3)[3]-OLSc3[3,2])*x2
yP<-coef(OLS4)[2]*x + coef(OLS4)[3]*x2
yPu<-(coef(OLS4)[2]+OLSc4[2,2])*x + (coef(OLS4)[3]+OLSc4[3,2])*x2
yPl<-(coef(OLS4)[2]-OLSc4[2,2])*x + (coef(OLS4)[3]-OLSc4[3,2])*x2

windows()
par(bty="l",family="serif")
plot(x,yW,type="n",xlab="Natural Log of Bilateral Aid at Year t-1",
     ylab="Natural Log of Bilateral Migrant Inflows at Year t",lwd=2,col="black")
abline(h=seq(0,0.5,.1),col="lightgrey")
abline(v=seq(0,8,2),col="lightgrey")
lines(x,yP,lty=1,col="grey50",lwd=2)
lines(x,yW,lty=1,col="grey35",lwd=2)
legend("top",bg="white",legend=c("Wealthiest Donors","Least Wealthy Donors"),col=c("grey35","grey50"),lwd=c(2,2),lty=1)

yAus<-coef(OLS3)[1998]*x + coef(OLS3)[2015]*x2
yBgl<-coef(OLS3)[1999]*x + coef(OLS3)[2016]*x2
yCan<-coef(OLS3)[2000]*x + coef(OLS3)[2017]*x2
yDmk<-coef(OLS3)[2001]*x + coef(OLS3)[2018]*x2
yEsp<-coef(OLS3)[2002]*x + coef(OLS3)[2019]*x2
yFld<-coef(OLS3)[2003]*x + coef(OLS3)[2020]*x2
yFrn<-coef(OLS3)[2004]*x + coef(OLS3)[2021]*x2
yGrm<-coef(OLS3)[2005]*x + coef(OLS3)[2022]*x2
yIty<-coef(OLS3)[2006]*x + coef(OLS3)[2023]*x2
yJpn<-coef(OLS3)[2007]*x + coef(OLS3)[2024]*x2
yNdl<-coef(OLS3)[2008]*x + coef(OLS3)[2025]*x2
yNwy<-coef(OLS3)[2009]*x + coef(OLS3)[2026]*x2
yNzd<-coef(OLS3)[2010]*x + coef(OLS3)[2027]*x2
ySwd<-coef(OLS3)[2011]*x + coef(OLS3)[2028]*x2
ySwz<-coef(OLS3)[2012]*x + coef(OLS3)[2029]*x2
yUk<-coef(OLS3)[2013]*x + coef(OLS3)[2030]*x2
yUs<-coef(OLS3)[2014]*x + coef(OLS3)[2031]*x2

windows()
par(bty="n",family="serif")
plot(x,yAus,xlab="Natural Log of Bilateral Aid at Year t-1",
     ylab="Natural Log of Bilateral Migrant Inflows at Year t",type="l",lwd=1,col=alpha("black",0.15),
     ylim=c(-10,10))
lines(x,yBgl,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yCan,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yDmk,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yEsp,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yFld,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yFrn,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yGrm,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yIty,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yJpn,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yNdl,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yNwy,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yNzd,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,ySwd,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,ySwz,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yUk,lty=1,lwd=1,col=alpha("black",0.15))
lines(x,yUs,lty=1,lwd=1,col=alpha("black",0.15))

## Display Results
library(dotwhisker)
term<-c(1,3)
termName<-c("Log(BILATERAL AID)","Log(BILATERAL AID)^2")
estimate<-c(unique(OLSc2[2:3,1]))
std.error<-c(unique(OLSc2[2:3,2]))
windows()
par(mfcol=c(1,2))
par(bty="l",family="serif",xaxt="n",
    mar=c(11,4,4,2)+0.1)
plot(term,estimate,type="n",xlim=c(0,3.5),
     ylim=c(-0.01,0.09),
     xlab="",ylab="OLS Estimates for Bilateral Migrant Inflows")
abline(v=seq(1,3,2),col="lightgrey")
abline(h=seq(0,0.08,0.02),col="lightgrey")
abline(h=0,lty=1,lwd=1)
segments(term,estimate+std.error,term,estimate-std.error)
points(term,estimate+std.error,bg="grey",pch=25)
points(term,estimate-std.error,bg="grey",pch=24)
points(term,estimate,pch=19)
par(xaxt="s")
axis(1,at=c(1,3),labels=c(termName),las=2)

estimate<-c(coeftest(tob2)[3:4,1])
std.error<-c(coeftest(tob2)[3:4,2])
par(bty="l",family="serif",xaxt="n",
    mar=c(11,4,4,2)+0.1)
plot(term,estimate,type="n",xlim=c(0,3.5),
     ylim=c(-0.01,0.09),
     xlab="",ylab="Tobit Estimates for Bilateral Migrant Inflows")
abline(v=seq(1,3,2),col="lightgrey")
abline(h=seq(0,0.08,0.02),col="lightgrey")
abline(h=0,lty=1,lwd=1)
segments(term,estimate+std.error,term,estimate-std.error)
points(term,estimate+std.error,bg="grey",pch=25)
points(term,estimate-std.error,bg="grey",pch=24)
points(term,estimate,pch=19)
par(xaxt="s")
axis(1,at=c(1,3),labels=c(termName),las=2)

mod1<-data.frame(term,estimate,std.error)
mod1$model<-c("OLS Estimates\n(Robust Clustered Standard Errors Shown)")
estimate<-coeftest(tob2)[3:4,1]
std.error<-coeftest(tob2)[3:4,2]
mod2<-data.frame(term,estimate,std.error)
mod2$model<-c("Tobit Estimates\n(Classic Standard Errors Shown)")
mods<-rbind(mod1,mod2)
windows()
dwplot(mods,dot_args = aes(size=2),
           whisker_args = aes(size=.75))  + 
  geom_vline(xintercept = 0, colour = "black", linetype = 1) + theme_minimal() +
  scale_color_grey() +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  xlab("Effect on Bilateral Migrant Inflows") + theme(text=element_text(size=12, family="serif")) +
  theme(axis.text.y=element_text(color="black",size=10)) +
  theme(axis.text.x=element_text(color="black")) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_line(color="grey50",linetype=3))+
  theme(panel.grid.minor.x=element_line(color="grey75",linetype=3)) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=2))

## Robust Check
library(VGAM)
tob1<-vglm(logrInflowLead~logdAid+logrInflow+logdrDistance+drColony+drLanguage+
            dRestrict+logdGDPcap+dUnemployment+logdPopulation+
            rConflict+rDemo+as.factor(Donor)+as.factor(Recipient)+as.factor(year),tobit(Lower = 0),
          data=immig)
tob2<-vglm(logrInflowLead~logdAid+logdAid2+logrInflow+logdrDistance+drColony+drLanguage+
             dRestrict+logdGDPcap+dUnemployment+logdPopulation+
             rConflict+rDemo+as.factor(Donor)+as.factor(Recipient)+as.factor(year),tobit(Lower = 0),
           data=immig)
tob3<-vglm(logrInflowLead~logdAid+logdAid2+logrInflow+logdrDistance+drColony+drLanguage+
             dRestrict+logdGDPcap+dUnemployment+logdPopulation+
             rConflict+rDemo+as.factor(Donor)+as.factor(Recipient)+as.factor(year),tobit(Lower = 0),
           data=immig2)
tob4<-vglm(logrInflowLead~logdAid+logdAid2+logrInflow+logdrDistance+drColony+drLanguage+
             dRestrict+logdGDPcap+dUnemployment+logdPopulation+
             rConflict+rDemo,tobit(Lower = 0),
           data=immig3)

d<-density(immig$logrInflowLead)
windows()
par(bty="l",family="serif")
plot(d,xlab="Natural Log of Bilateral Migrant Inflows\nN = 19,081  Bandwidth = 0.3147",
     ylab="Count",main="",xlim=c(0,14),type="n")
abline(h=seq(0,0.2,0.05),col="lightgrey")
abline(v=seq(0,14,2),col="lightgrey")
polygon(d,col="darkgrey", border="black") 
abline(v=median(immig$logrInflowLead))

# Create Tables
write.csv(coeftest(tob1)[1:13,],"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/tob1.csv")
write.csv(coeftest(tob2)[1:14,],"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/tob2.csv")
write.csv(coeftest(tob3)[1:14,],"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/tob3.csv")


X<-summary(data.frame(immig$rInflowLead,immig$dAid))

write.csv(X,"C:/Users/Miles/Documents/R/Immigration and Aid/Immigration and Aid/summary.csv")

## Display Donor Fixed Effects
library(dotwhisker)
term<-c(1:17)
termName<-c("Australia","Belgium","Canada","Denmark","Spain","Finland","France","Germany","Italy",
        "Japan","The Netherlands","Norway","New Zealand","Sweden","Switzerland","United Kingdom",
        "United States")
estimate<-OLSc2[14:30,1]
std.error<-OLSc2[14:30,2]
windows()
par(bty="l",family="serif",xaxt="n",
    mar=c(7,4,4,2)+0.1)
plot(term,estimate,type="n",ylim=c(-3,9),
     xlab="",ylab="Estimates for Bilateral Migrant Inflows")
abline(v=seq(1,17,1),col="lightgrey")
abline(h=seq(-2,8,2),col="lightgrey")
abline(h=0,lty=1,lwd=1)
segments(term,estimate+std.error,term,estimate-std.error)
points(term,estimate+std.error,bg="grey",pch=25)
points(term,estimate-std.error,bg="grey",pch=24)
points(term,estimate,pch=19)
par(xaxt="s")
axis(1,at=c(1:17),labels=c(termName),las=2)

windows()
par(bty="l",family="serif",yaxt="n",
    mar=c(5,7,4,2)+0.1)
plot(estimate,term,type="n",xlim=c(-3,9),
     ylab="",xlab="Effect on Bilateral Migrant Inflows")
abline(h=seq(1,17,1),col="lightgrey")
abline(v=seq(-2,8,2),col="lightgrey")
abline(v=0,lty=1,lwd=1)
segments(estimate+std.error-0.02,term,estimate-std.error,term)
points(estimate,term,pch=20,cex=.9)
points(estimate+std.error,term,bg="grey",pch="|",cex=1.5)
points(estimate-std.error,term,bg="grey",pch="|",cex=1.5)
par(yaxt="s")
axis(2,at=c(1:17),labels=c(termName),las=2)

dwplot(mod,dot_args = aes(size=2),
       whisker_args = aes(size=.75))  + 
  geom_vline(xintercept = 0, colour = "black", linetype = 1) + theme_minimal() +
  scale_color_grey() +
  theme(legend.position = "none") +
  xlab("Effect on Bilateral Migrant Inflows") + theme(text=element_text(size=12, family="serif")) +
  theme(axis.text.y=element_text(color="black",size=10)) +
  theme(axis.text.x=element_text(color="black")) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_line(color="grey50",linetype=3))+
  theme(panel.grid.minor.x=element_line(color="grey75",linetype=3)) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=2))

## Display Year Fixed Effects
library(dotwhisker)
term<-c("Australia","Belgium","Canada","Denmark","Spain","Finland","France","Germany","Italy",
        "Japan","The Netherlands","Norway","New Zealand","Sweden","Switzerland","United Kingdom",
        "United States")
estimate<-OLSc3[1998:2014,1]
std.error<-OLSc3[1998:2014,2]
mod1<-data.frame(term,estimate,std.error)
mod1$model<-c("Interaction of Bilateral Aid\nwith Donor Indicator Variable")
term<-c("Australia","Belgium","Canada","Denmark","Spain","Finland","France","Germany","Italy",
        "Japan","The Netherlands","Norway","New Zealand","Sweden","Switzerland","United Kingdom",
        "United States")
estimate<-OLSc3[2015:2031,1]
std.error<-OLSc3[2015:2031,2]
mod2<-data.frame(term,estimate,std.error)
mod2$model<-c("Interaction of Quadratic Term\nfor Aid with Donor Indicator Variable")
mods<-rbind(mod1,mod2)
windows()
dwplot(mods,dot_args = aes(size=2),
       whisker_args = aes(size=.75))  + 
  geom_vline(xintercept = 0, colour = "black", linetype = 1) + theme_minimal() +
  scale_color_grey() +
  theme(legend.position = "bottom") +
  theme(legend.title=element_blank()) +
  xlab("Effect on Bilateral Migrant Inflows") + theme(text=element_text(size=12, family="serif")) +
  theme(axis.text.y=element_text(color="black",size=10)) +
  theme(axis.text.x=element_text(color="black")) +
  theme(panel.grid.minor.y=element_blank()) +
  theme(panel.grid.major.x=element_line(color="grey50",linetype=3))+
  theme(panel.grid.minor.x=element_line(color="grey75",linetype=3)) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=2))

term<-c(1995:2011)
estimate<-coeftest(tob2)[161:177,1]
std.error<-coeftest(tob2)[161:177,2]
windows()
par(bty="l",family="serif",xaxt="n")
plot(term,estimate,type="n",ylim=c(-0.2,0.45),
     xlab="Year",ylab="Estimates for Bilateral Migrant Inflows")
abline(h=seq(-0.2,.4,0.1),col="lightgrey")
abline(v=seq(1995,2011,1),col="lightgrey")
abline(h=0,lty=1,lwd=1)
segments(term,estimate+std.error,term,estimate-std.error)
points(term,estimate,pch=19)
points(term,estimate+std.error,bg="grey",pch=25)
points(term,estimate-std.error,bg="grey",pch=24)
par(xaxt="s")
axis(1,at=c(1995:2011),labels=c(term),las=2)
