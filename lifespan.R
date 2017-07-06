# max. human lifespan analysis
# data from Figure 2a, Dong et al. 2016 Nature

# data
lspan <- structure(list(year = c(1968L, 1970L, 1973L, 1975L, 1978L, 1979L, 1980L, 1981L, 1982L, 1983L, 1984L, 1985L, 1986L, 1987L, 1988L, 1989L, 1990L, 1991L, 1992L, 1993L, 1994L, 1995L, 1996L, 1997L, 1998L, 1999L, 2000L, 2001L, 2002L, 2003L, 2004L, 2005L, 2006L), max.reported.age.death = c(111L, 111L, 112L, 111L, 111L, 110L, 111L, 113L, 113L, 113L, 111L, 114L, 113L, 114L, 114L, 112L, 112L, 112L, 114L, 117L, 115L, 112L, 114L, 122L, 115L, 119L, 114L, 115L, 115L, 114L, 113L, 114L, 112L)), .Names = c("year", "max.reported.age.death"), class = "data.frame", row.names = c(NA, -33L))
lspan

mod1 <- lm(max.reported.age.death ~ year, data=subset(lspan, year<1995)) # pre-1995 analysis from paper
mod2 <- lm(max.reported.age.death ~ year, data=subset(lspan, year>1994)) # post-1994 analysis from paper
mod3 <- lm(max.reported.age.death ~ year, data=subset(lspan, year>1994 & year!=1997)) # post-1994, outlier removed
summary(mod1) # y = 0.1531x -191.07; R2 = 0.46
summary(mod2) # y = -0.2762x + 667.50; R2 = 0.12
# mod1 and mod2 fits match Figure 2a, confirming that the data is entered correctly
summary(mod3) # fit with the 1997 outlier removed

# permute the data, leaving out the outlier
# how often are the pos/neg slopes achieved?

nreps <- 10000
results <- data.frame(slope1=rep(NA, nreps), slope2=NA, int1=NA, int2=NA)
for(i in 1:nreps){
  shuff <- lspan[lspan$year!=1997,]
  shuff$year <- sample(shuff$year)
  # shuff <- rbind(shuff, lspan[24,]) # leave out the outlier, 1997
  modA <- lm(max.reported.age.death ~ year, data=subset(shuff, year<1995))
  modB <- lm(max.reported.age.death ~ year, data=subset(shuff, year>1994))
  results$slope1[i] <- summary(modA)$coef[2,'Estimate']
  results$slope2[i] <- summary(modB)$coef[2,'Estimate']
  results$int1[i] <- summary(modA)$coef[1,'Estimate']
  results$int2[i] <- summary(modB)$coef[1,'Estimate']
  print(i)
}

dev.new(width=5, height=4)
plot(max.reported.age.death ~ year, lspan, las=1, pch=16) # add first 1000 fits
for(i in 1:1000) segments(x0=c(1968), y0=c(results$int1[i] + results$slope1[i]*1968), x1=c(1994), y1=c(results$int1[i] + results$slope1[i]*1994), lwd=0.25, col=rgb(0,0,0,0.05))
for(i in 1:1000) segments(x0=c(1995), y0=c(results$int2[i] + results$slope2[i]*1995), x1=c(2006), y1=c(results$int2[i] + results$slope2[i]*2006), lwd=0.25, col=rgb(0,0,0,0.05))
segments(x0=1968, x1=1994, y0=summary(mod1)$coef[1,1] + 1968*summary(mod1)$coef[2,1], y1=summary(mod1)$coef[1,1] + 1994*summary(mod1)$coef[2,1], col='blue')
segments(x0=1995, x1=2006, y0=summary(mod2)$coef[1,1] + 1995*summary(mod2)$coef[2,1], y1=summary(mod2)$coef[1,1] + 2006*summary(mod2)$coef[2,1], col='orange')
segments(x0=1995, x1=2006, y0=summary(mod3)$coef[1,1] + 1995*summary(mod3)$coef[2,1], y1=summary(mod3)$coef[1,1] + 2006*summary(mod3)$coef[2,1], col='brown') # no outlier
legend('topleft', lwd=c(1,1,1,0.25), col=c('blue','orange','brown','black'), legend=c('pre-1995 actual', 'post-1994 actual', 'post-1994, no outlier','randomized'), bty='n', cex=0.5)

dev.new(width=5, height=6)
par(mfrow=c(2,1), las=1, mar=c(3,3,2,0.25))
hist(results$slope1, main='slope pre-1995', xlab='b', ylim=c(0,3000), xlim=c(-1,1)); abline(v=summary(mod1)$coef[2,1], col='blue')
polygon(y=c(0,3000,3000,0), x=c(rep(summary(mod1)$coef[2,1] - 1.96*summary(mod1)$coef[2,2], 2), rep(summary(mod1)$coef[2,1] + 1.96*summary(mod1)$coef[2,2], 2)), col=rgb(0,0,1,0.25), border=NA)
legend('topright', col='blue', lwd=1, legend='actual slope +/- 95% CI', bty='n', cex=0.5)
hist(results$slope2, main='slope post-1994', xlab='b', ylim=c(0,3000), xlim=c(-1,1)); abline(v=summary(mod2)$coef[2,1], col='orange')
polygon(y=c(0,3000,3000,0), x=c(rep(summary(mod2)$coef[2,1] - 1.96*summary(mod2)$coef[2,2], 2), rep(summary(mod2)$coef[2,1] + 1.96*summary(mod2)$coef[2,2], 2)), col=rgb(1,0.5,0,0.25), border=NA)
abline(v=summary(mod3)$coef[2,1], col='brown')
polygon(y=c(0,3000,3000,0), x=c(rep(summary(mod3)$coef[2,1] - 1.96*summary(mod3)$coef[2,2], 2), rep(summary(mod3)$coef[2,1] + 1.96*summary(mod3)$coef[2,2], 2)), col=rgb(0.5,0.1,0.1,0.25), border=NA)
legend('topright', col=c('orange','brown'), lwd=1, legend=c('actual slope +/- 95% CI', 'slope no outlier'), bty='n', cex=0.5)

# post-1994 slope is consistent with randomized data AND with the pre-1995 slope estimate, regardless of whether the outlier for 1997 is included
