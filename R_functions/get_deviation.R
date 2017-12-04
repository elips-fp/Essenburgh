get_deviations <- function(vector)
{
  mean <- mean(vector, na.rm=T)
  sd <- sd(vector, na.rm=T)
  
  sd_vector <- rep(0, length(vector))
  
  for(i in 1:3)
  {
    minus_sd <- which(vector < mean-(i*sd))
    plus_sd <- which(vector > mean+(i*sd))
    
    sd_vector[minus_sd] <- paste('-',i,sep='')
    sd_vector[plus_sd] <- paste('+',i, sep='')
  }
  
  return(sd_vector)
}