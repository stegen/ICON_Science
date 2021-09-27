
dat = read.csv("ICON Cost_Benefit - Sheet1.csv",skip=1,stringsAsFactors = F)
head(dat)

# make kernel density plots

pdf("ICON_Cost_Benefit.pdf",height=10)
par(pty="s",mfrow=c(2,1),mar=c(4,4,1,1))

plot(density(dat$I_Cost,from = 0,to = 10),type = "l",lwd=2,xlab="Cost",cex.lab=2,cex.axis=1.5,main="",ylim=c(0,0.3))
points(density(dat$C_Cost,from = 0,to = 10),type = "l",lwd=2,col=2)
points(density(dat$O_Cost,from = 0,to = 10),type = "l",lwd=2,col=4)
points(density(dat$N_Cost,from = 0,to = 10),type = "l",lwd=2,col="orange")
legend(0,0.3,legend = c(paste0("I (",round(median(dat$I_Cost),digits = 2),")"),
                        paste0("C (",round(median(dat$C_Cost),digits = 2),")"),
                        paste0("O (",round(median(dat$O_Cost),digits = 2),")"),
                        paste0("N (",round(median(dat$N_Cost),digits = 2),")")),
       lty = 1,col=c(1,2,4,"orange"),lwd=4)
mtext(text = "(a) ",side = 3,line = -1.5,cex = 1.5,adj = 1)


plot(density(dat$I_Benefit,from = 0,to = 10),type = "l",lwd=2,xlab="Benefit",cex.lab=2,cex.axis=1.5,main="",ylim=c(0,0.9))
points(density(dat$C_Benefit,from = 0,to = 10),type = "l",lwd=2,col=2)
points(density(dat$O_Benefit,from = 0,to = 10),type = "l",lwd=2,col=4)
points(density(dat$N_Benefit,from = 0,to = 10),type = "l",lwd=2,col="orange")
legend(0,0.9,legend = c(paste0("I (",round(median(dat$I_Benefit),digits = 2),")"),
                        paste0("C (",round(median(dat$C_Benefit),digits = 2),")"),
                        paste0("O (",round(median(dat$O_Benefit),digits = 2),")"),
                        paste0("N (",round(median(dat$N_Benefit),digits = 2),")")),
       lty = 1,col=c(1,2,4,"orange"),lwd=4)
mtext(text = "(b) ",side = 3,line = -1.5,cex = 1.5,adj = 1)

dev.off()

# do wilcox text comparing cost to benefit

costs = c(dat$I_Cost,dat$C_Cost,dat$O_Cost,dat$N_Cost)

benefits = c(dat$I_Benefit,dat$C_Benefit,dat$O_Benefit,dat$N_Benefit)

summary(costs)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.750   3.750   5.375   5.434   7.250   9.250

summary(benefits)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.750   7.750   8.375   8.177   9.000   9.500 

wilcox.test(x = costs,y = benefits)
# W = 2273.5, p-value < 2.2e-16


