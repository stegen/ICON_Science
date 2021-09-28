
dat = read.csv("ICON Cost_Benefit - Sheet1.csv",skip=1,stringsAsFactors = F)
head(dat)

# compute the cost-benefit ratios for each letter in each section

dat$I_CB_Ratio = dat$I_Cost/dat$I_Benefit
dat$C_CB_Ratio = dat$C_Cost/dat$C_Benefit
dat$O_CB_Ratio = dat$O_Cost/dat$O_Benefit
dat$N_CB_Ratio = dat$N_Cost/dat$N_Benefit

# make kernel density plots

pdf("ICON_Cost_Benefit.pdf",height=15)
par(pty="s",mfrow=c(3,1),mar=c(4,4,1,1))

plot(density(dat$I_Cost,from = 0,to = 10),type = "l",lwd=2,xlab="Cost",cex.lab=3,cex.axis=2,main="",ylim=c(0,0.3))
points(density(dat$C_Cost,from = 0,to = 10),type = "l",lwd=2,col=2)
points(density(dat$O_Cost,from = 0,to = 10),type = "l",lwd=2,col=4)
points(density(dat$N_Cost,from = 0,to = 10),type = "l",lwd=2,col="orange")
legend(0,0.3,legend = c(paste0("I (",round(median(dat$I_Cost),digits = 2),")"),
                        paste0("C (",round(median(dat$C_Cost),digits = 2),")"),
                        paste0("O (",round(median(dat$O_Cost),digits = 2),")"),
                        paste0("N (",round(median(dat$N_Cost),digits = 2),")")),
       lty = 1,col=c(1,2,4,"orange"),lwd=4,cex=2)
mtext(text = "(a) ",side = 3,line = -2.5,cex = 1.5,adj = 1)


plot(density(dat$I_Benefit,from = 0,to = 10),type = "l",lwd=2,xlab="Benefit",cex.lab=3,cex.axis=2,main="",ylim=c(0,0.9))
points(density(dat$C_Benefit,from = 0,to = 10),type = "l",lwd=2,col=2)
points(density(dat$O_Benefit,from = 0,to = 10),type = "l",lwd=2,col=4)
points(density(dat$N_Benefit,from = 0,to = 10),type = "l",lwd=2,col="orange")
legend(0,0.9,legend = c(paste0("I (",round(median(dat$I_Benefit),digits = 2),")"),
                        paste0("C (",round(median(dat$C_Benefit),digits = 2),")"),
                        paste0("O (",round(median(dat$O_Benefit),digits = 2),")"),
                        paste0("N (",round(median(dat$N_Benefit),digits = 2),")")),
       lty = 1,col=c(1,2,4,"orange"),lwd=4,cex=2)
mtext(text = "(b) ",side = 3,line = -2.5,cex = 1.5,adj = 1)

plot(density(dat$I_CB_Ratio,from = 0,to = 2),type = "l",lwd=2,xlab="Cost-Benefit Ratio",cex.lab=3,cex.axis=2,main="",ylim=c(0,1.6))
points(density(dat$C_CB_Ratio,from = 0,to = 2),type = "l",lwd=2,col=2)
points(density(dat$O_CB_Ratio,from = 0,to = 2),type = "l",lwd=2,col=4)
points(density(dat$N_CB_Ratio,from = 0,to = 2),type = "l",lwd=2,col="orange")
legend(1.2,1.5,legend = c(paste0("I (",round(median(dat$I_CB_Ratio),digits = 2),")"),
                        paste0("C (",round(median(dat$C_CB_Ratio),digits = 2),")"),
                        paste0("O (",round(median(dat$O_CB_Ratio),digits = 2),")"),
                        paste0("N (",round(median(dat$N_CB_Ratio),digits = 2),")")),
       lty = 1,col=c(1,2,4,"orange"),lwd=4,cex=2)
abline(v=1,col=8,lwd=2,lty=2)
mtext(text = "(c) ",side = 3,line = -2.5,cex = 1.5,adj = 1)


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

# do one sided wilcox to test if medians of the ratios are less than 1
wilcox.test(x = dat$I_CB_Ratio,alternative = "less",mu = 1)
# data:  dat$I_CB_Ratio
# V = 21, p-value = 1.79e-05
# alternative hypothesis: true location is less than 1

wilcox.test(x = dat$C_CB_Ratio,alternative = "less",mu = 1)
# data:  dat$C_CB_Ratio
# V = 14, p-value = 5.66e-06
# alternative hypothesis: true location is less than 1

wilcox.test(x = dat$O_CB_Ratio,alternative = "less",mu = 1)
# data:  dat$O_CB_Ratio
# V = 6, p-value = 1.669e-06
# alternative hypothesis: true location is less than 1

wilcox.test(x = dat$N_CB_Ratio,alternative = "less",mu = 1)
# data:  dat$N_CB_Ratio
# V = 55, p-value = 0.0001357
# alternative hypothesis: true location is less than 1

# doing PCA and associated plot
library(ggfortify)

df = dat[,-grep(pattern = "Ratio",x = colnames(dat))]
df = df[,-grep(pattern = "Team",x = colnames(df))]

pca_res <- prcomp(df[,-grep(pattern = "Section",x = colnames(df))], scale. = F)

pdf("ICON_PCA.pdf", width = 12)
autoplot(pca_res, data = df, colour = 'Section', loadings = TRUE,
         loadings.label = TRUE, loadings.label.size = 3,loadings.colour = 'black')
dev.off()


