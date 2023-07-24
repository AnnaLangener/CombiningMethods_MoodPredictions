
################################################
#### Function to create train and test sets ####
################################################


BlockedCV <- function(data, origin, horizon){
  samplesize <- nrow(data)
  trainindex <- list()
  testindex <- list()
  
  trainindex[[1]] <- 1:origin
  testindex[[1]] <- (origin + 1):(origin+horizon)
  
  counter <- 1
  index <- testindex[[1]][horizon]+1
  
  while(testindex[[counter]][horizon]+1 < (samplesize-(origin+horizon))){
  index <- testindex[[counter]][horizon]+1
  trainindex[[counter+1]] <- index:(index+origin)
  testindex[[counter+1]] <-(index+origin+1):(index+origin+horizon)
  counter <- counter + 1
}
  return(list(trainindex,testindex))
}


SlidingWindow_CV <- function(data, origin, horizon){
  samplesize <- nrow(data)
  trainindex <- list()
  testindex <- list()
  
  trainindex[[1]] <- 1:origin
  testindex[[1]] <- (origin + 1):(origin+horizon)
  
  counter <- 1
  index <- testindex[[1]][horizon]+1
  
  while(testindex[[counter]][horizon] < (samplesize-horizon)){
    index <- testindex[[counter]][horizon]+1
    trainindex[[counter+1]] <- (index-origin+1):index-1
    testindex[[counter+1]] <- (index):(index+horizon-1)
    counter <- counter + 1
  }
  return(list(trainindex,testindex))
}

SlidingWindow_CV(Participant1,5,2)
BlockedCV(Participant1,20,10)

