l <- vector('list',nrow(df))
matches <- list(mother = l,father = l)
for(i in 1:nrow(df)){
  father_id <- with(df,which(student_name[i] == father_name))
  if(length(father_id) == 1){
    matches[['father']][[i]] <- father_id
  } else {
    old_father_id <- NULL
    ## try to find the total                                                                                                                                 
    for(m in 10:1){ ## m is the maximum distance                                                                                                             
      father_id <- with(df,agrep(student_name[i],father_name,max.dist = m))
      if(length(father_id) == 1 || m == 1){
        ## if we find a unique match or if we are in our last round, then stop                                                                               
        matches[['father']][[i]] <- father_id
        break
      } else if(length(father_id) == 0 && length(old_father_id) > 0) {
        ## if we can't do better than multiple matches, then record them anyway                                                                              
        matches[['father']][[i]] <- old_father_id
        break
      } else if(length(father_id) == 0 && length(old_father_id) == 0) {
        ## if the nearest match is more than 10 different from the current pattern, then stop                                                                
        break
      }
    }
  }
}
