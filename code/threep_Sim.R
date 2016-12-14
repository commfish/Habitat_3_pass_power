#This function is needed for the Sim function. It is very similiar to the threep function
#but has a different input
threep_sim <- function(C,k){

#model inputs
T <- sum(C[])
Xc <- C
for (i in 1:k){
Xc[i] <- (k - i)*C[i]}
X <- sum(Xc[])
alpha <- 1
beta <- 1
i <- 1:k
N0 <- T
#Calculate N
while (((N0+1)/(N0-T+1))*prod((k*N0-X-T+beta+k-i)/(k*N0-X+alpha+beta+k-i)) >= 1.0) { N0 <- N0+1 }
#Calculate p
p <- T/(k*N0-X)
  
#Model outputs  
N0.var <- (N0*(N0-T)*T)/((T^2)-N0*(N0-T)*(((k*p)^2)/(1-p)))
N0.se <- sqrt(N0.var)
N0r <- round(N0, digits=0)
N0.varr <- round(N0.var, digits=2)
N0.ser <- format(N0.se, digits=2)

#If you use the N0r version of the variables, the Sim function does not work
outlist <- as.vector(c(N0, N0.var, N0.se, p))
return(outlist)
}

