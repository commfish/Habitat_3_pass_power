
threep <- function(data){
	#A function for calculating three pass population estimates based upon the Carle Strub methods
	#Ben Williams 2015/2016
	#Alaska Department of Fish and Game
	
	#Load packages
	require(tidyverse)
	#Pull out necessary data totals
	data %>% group_by(pass) %>% summarise(catch=sum(catch)) -> dat1
	
	#model inputs
		k <- length(dat1$pass)
		i <- 1:k
		X <- sum((k-dat1$pass)*dat1$catch)
		T <- sum(dat1$catch)
		Xc1<-dat1[dat1$pass == 1,]
		Xc1<-sum(Xc1$catch)
		Xc2<-dat1[dat1$pass == 2,]
		Xc2<-sum(Xc2$catch)
		Xc3<-dat1[dat1$pass == 3,]
		Xc3<-sum(Xc3$catch)
		Xc<-as.vector(c(Xc1, Xc2, Xc3))
	
		alpha <- 1
		beta <- 1
		N0 <- T
	#Calculate N
		while (((N0+1)/(N0-T+1))*prod((k*N0-X-T+beta+k-i)/(k*N0-X+alpha+beta+k-i)) >= 1.0) { N0 <- N0+1 }
	#Calculate p
		p <- T/(k*N0-X)
	#Model outputs  
		a<-((N0)*(N0-T)*(T))
		c<-((k*p))^2/(1-p)
		b<-(T^2)-N0*(N0-T)*c
		N0.var <- a/b
		N0.se <- sqrt(N0.var)
		N0r <- round(N0, digits=0)
		N0.varr <- round(N0.var, digits=2)
		N0.ser <- format(N0.se, digits=2)
		p <- round(p, digits=2)
		mleN <- round(N0*4, digits=0)
		ll <- N0-N0.se*2
		ul <- N0+N0.se*2
		mleLL <- round(mleN - (N0-ll) * 4, digits=2)
		mleUL <- round(mleN + (ul-N0) * 4, digits=2)
	
	#Estimated catches
	Ec <- c(N0*p, N0*(1-p)*p, N0*(1-p)^2*p)
	
	#Chi Square test inputs and results
	x2 <- round(sum(((dat1$catch-Ec)^2)/Ec), digits=2)
	x2.crit <- round(qchisq(.95, df = 1), digits=2)
	x2.p <- round(pchisq(x2,1), digits=4)
	
	#data for linear regression and plotting
	data.plot <- data.frame(tot=c(0, dat1$catch[1], dat1$catch[1]+dat1$catch[2]), catch=dat1$catch)
	m1 <- lm(catch~tot, data=data.plot)
	data.plot <- data.frame(tot=c(0, dat1$catch[1], dat1$catch[1]+dat1$catch[2], coef(m1)[1]/-coef(m1)[2]), catch=c(dat1$catch,0))
	data.plot$pred <- predict(m1, data.plot)
	popest <- round(max(data.plot$tot))
	
	#generate linear regression plot
	gplot <- ggplot(data.plot, aes( tot, catch))+geom_point()+geom_line(aes(tot, pred)) +
		geom_text(aes(x=max(tot), y=min(catch), label= popest, vjust=-10)) +
		geom_text(aes(x=max(tot), y=min(catch), label= "Pop Est =", vjust=-10, hjust=1.2)) + 
		xlab("Sum of previous catches")+ylab("Catches")+theme_bw() +
		theme(axis.line = element_line(colour = "black"),
				panel.grid.major = element_blank(),
				panel.grid.minor = element_blank()) 
	
	#output variables
	out <- list('N' = N0r,'N2' = mleN, 'N.var' = N0.varr, 'p' = p,  'N.se' = N0.ser, 'mleLL' = mleLL, 'mleUL' = mleUL, 'x2' = x2, 
					'x2.crit' = x2.crit, 'x2.p' = x2.p, 'Popest' = popest, "data.plot" = data.plot, 'gplot' = gplot, 'k'= k, 'C'=Xc, 
					'a' = a,'b' = b, 'c' = c)
}	
