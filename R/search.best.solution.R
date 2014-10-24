#'Search for the best solutions
#'
#'Search for the best solutions  
#'
#'@param solutio
#'@return best solutions
#' 
#'@author Tabakov Konstantin
#'
search.best.solution = function( solution ) {
  vec.min.val = c()
  
  for( i in 1 : length( solution ) ) {
    vec.min.val = c(vec.min.val, solution[[i]][[2]])
  }
  
  return(solution[[which.min(vec.min.val)]])
  
}