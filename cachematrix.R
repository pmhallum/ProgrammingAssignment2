## The following two function work in tandem to reduce required
#  computations by saving the calculated inverse of a non-degenerate
#  matrix, A, and only recalculating the inverse if the matrix, A, 
#  has changed.

## Function creating list of four items to
#  1) set the value of the matrix,
#  2) get the value of the matrix,
#  3) set the value of the matrix inverse,
#  4) get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  cheXInv <- NULL
  
  # 1) define "set function"
  set <- function(y){
           x <<- y
           cheXInv <<- NULL
         }
  # 2) define "get function"
  get <- function() x
  
  # 3) define "set Inverse function"
  setInv <- function(calcInverse) cheXInv <<- calcInverse
  
  # 4) define "get Inverse function"
  getInv <- function() cheXInv
  
  # 5) create/output list
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function calculates the inverse of a "matrix object" 
#  (as defined by the previous function) only if the underlying
#  input matrix (A, above) has been changed.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  cheXInv <- x$getInv()
  
  # If a matrix inverse already exists for this matrix, retrieve
  # existing inverse (and note so).
  if (!is.null(cheXInv)){
    message("Retrieving cached data.")
    return(cheXInv) #exits function here if inverse found
  }
  
  # Else, calculate new matrix inverse, output, and save.
  data <- x$get()
  cheXInv <- solve(data)
  x$setInv(cheXInv)
  cheXInv
}
