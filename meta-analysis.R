install.packages("gmeta")
library(gmeta)

data(ulcer)
ulcer.o <- as.matrix(ulcer)

# impute 0.5
ulcer <- ifelse(ulcer.o == 0, 0.5, ulcer.o)
# summary statistics 
ulcer.theta <- log( (ulcer[,1]*ulcer[,4]) / (ulcer[,2]*ulcer[,3]) ) 
ulcer.sigma <- sqrt(1/ulcer[,1] + 1/ulcer[,2] + 1/ulcer[,3] + 1/ulcer[,4]) 
ulcer.pivots = data.frame(mns=ulcer.theta, sds=ulcer.sigma)

# fixed-effect model
gmo.mdlfx <- gmeta(ulcer.pivots, method='fixed-mle', gmo.xgrid=seq(from=-10,to=10,by=0.01))
summary(gmo.mdlfx)

plot(gmo.mdlfx, studies=c(4,8,15,16,23,41))
