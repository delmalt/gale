#' My awesome function
#'
#' @param matrix for males, females, and which goes first, and output produced
#' @return void
#'
#' @examples
#' gale_sh(a, b, F, F)
#'
#' @author Dalya Elmalt
#' @export
gale_sh <- function(male, female, female_proposal=F, outputDF=F){
  if(female_proposal==T){
    proposers <- female
    accepters <- male
  }
  else {
    proposers <- male
    accepters <- female
  }
  matches <- matrix(0,dim(proposers)[1],dim(accepters)[1])
  past_matches <- matrix(1,dim(proposers)[1],dim(accepters)[1])
  
  while(all(matches==past_matches)==F){
    
    past_matches <- matches
    
    for(i in 1:dim(proposers)[1]){
      for(j in order(proposers[i,])){ # goes over in order of preference
        
        if(sum(matches[i,])==0 & sum(matches[,j])==0){ # If not engaged, match!
          matches[i,j] <- 1
        }
        
        # If female is engaged, then I check if the new offer is more 
        # preferred than the one she is currently enagaged to, if so, the 
        # previous suitor is set free again and the new suitor becomes engaged
        # to her
        
        if(sum(matches[i,])==0 & sum(matches[,j])==1){ 
          past_prop <- which(matches[,j]==1) 
          if (accepters[j,i] < accepters[j,past_prop]){
            matches[past_prop,j] <- 0
            matches[i,j] <- 1
          }
        }
      }
    }
  }
  if(female_proposal==T){
    matches <- t(matches)
  }
  
  # I create the differnet variables that will become the data frame
  Man             <- matrix(0,20,1)
  Woman           <- matrix(0,20,1)
  rankMan_final   <- matrix(0,20,1)
  rankWoman_final <- matrix(0,20,1)
  
  if(outputDF==T){
    
    if(female_proposal==F){
      
      for(i in 1:dim(matches)[1]){
        for(j in 1:dim(matches)[2]){
          if(matches[i,j]==1){
            
            Man[i,1]             <- i
            Woman[i,1]           <- j
            rankMan_final[i,1]   <- which(proposers[i,]==j)
            rankWoman_final[i,1] <- which(accepters[j,]==i)}
        }
      }
      df <- data.frame(Man, Woman, rankMan_final, rankWoman_final)
      matches <- df
    }
    
    else{
      
      for(i in 1:dim(matches)[1]){
        for(j in 1:dim(matches)[2]){
          if(matches[i,j]==1){
            
            Man[i,1]             <- i
            Woman[i,1]           <- j
            rankMan_final[i,1]   <- which(accepters[i,]==j)
            rankWoman_final[i,1] <- which(proposers[j,]==i)}
        }
      }
      df <- data.frame(Man, Woman, rankMan_final, rankWoman_final)
      matches <- df
    }
  }
  
  return(matches) 
}