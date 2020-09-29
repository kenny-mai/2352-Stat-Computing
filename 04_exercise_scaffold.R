transpose_list <- function(x) {
  lengths <- rep(NA, length(x))
  for (i in 1:length(x))
    lengths[i] <- length(x[[i]])
  
  num.sub.elements <- lengths[1]
  
  if (!all(lengths == num.sub.elements))
    stop("all elements of x must have the same length")
  
  
  types <- matrix(NA, length(x), num.sub.elements)
  for (i in 1:length(x)) {
    for (j in 1:num.sub.elements) {
      types[i,j] <- typeof(x[[i]][[j]])
    }
  }
  for (j in 1:num.sub.elements) {
    if (!all(types[,j] == types[1,j])) {
      stop("all elements of the ", j, "th column must have the same type")
    }
  }
  
  results = matrix(nrow = length(x), ncol = length(x[[1]]))
  for (i in 1:length(x)) {
    results[i,] = unlist(x[[i]])
  }
  return(data.frame(results))
}

test_list <- list(list("red",   5, 22),
                  list("red",   7, 20),
                  list("black", 5, 22),
                  list("green", 6, 18))

transpose_list(test_list)




