# This function runs the simuulation for the power analysis
Sim <- function(N1,N2,pcap,nsims,k){
  nums <- as.vector(c(N1, N2))
  dimn <- length(nums)
  outprs <- (dimn-1)*(dimn/2)
  C <- vector(mode="numeric", len = k)

  for (i in 1:(dimn-1)){
    N1 <- nums[i]
    for (j in (i+1):dimn){
      N2 <- nums[j]
      outsim <- matrix(data=0, nrow=nsims, ncol=3)
      for (m in 1:nsims) {
        #
        #  Here we are sampling the first population
        #      
        N <- N1
        C[1] <- rbinom(1, N, pcap)
        N <- N1-C[1]
        C[2] <- rbinom(1, N, pcap)
        N <- N1-C[1]-C[2]
        C[3] <- rbinom(1, N, pcap)
        N1out <- threep_sim(C, k)
        #
        #  Here we are sampling the second population
        #      
        N <- N2
        C[1] <- rbinom(1, N, pcap)
        N <- N2-C[1]
        C[2] <- rbinom(1, N, pcap)
        N <- N2-C[1]-C[2]
        C[3] <- rbinom(1, N, pcap)
        N2out <- threep_sim(C, k)
        #
        #  Here we are comparing the population estimates and evaluating significance
        #  The 3 critical values below are based on a Student's t-distribution with 2 df
        #  for alpha = 0.05, alpha = 0.10, and alpha = 0.20 (2 tailed test).
        #  If more than 3 passes are used, these will need to be adjusted to reflect the
        #  number of degrees of freedom for the test statistic.  
        #
        teststat <- abs(N1out[1]-N2out[1])/(sqrt(N1out[2]+N2out[2]) + 0.00000000001)
        if(teststat >= 4.303) outsim[m,1] <- 1
        if(teststat >= 2.920) outsim[m,2] <- 1
        if(teststat >= 1.886) outsim[m,3] <- 1
      }
      #
      #   After 'nsims' iterations are completed for a pair of population sizes, summary
      #   statistics for the simulation and calculated power are written to the output
      #   matrix 'ondx'
      #
   pow95 <-sum(outsim[,1])/nsims
   pow90<- sum(outsim[,2])/nsims
   pow80<- sum(outsim[,3])/nsims
   pow95_percent<-pow95*100
   pow90_percent<-pow90*100
   pow80_percent<-pow80*100
  
    }
  }
  #output variables
  out <- list('N1' = N1,'N2' = N2, 'p' = pcap, 'pow95' = pow95, 'pow90' = pow90, 'pow80' = pow80,
              'pow95_percent' = pow95_percent, 'pow90_percent' = pow90_percent, 'pow80_percent' = pow80_percent)
}






