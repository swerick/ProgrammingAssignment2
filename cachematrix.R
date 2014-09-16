## MakeCacheMatrix  caches a matrix in physical memory
##      eg y <- MakeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
## $get() retrieves the object value        eg (y$get())
## $getmatrix() retrieves the object value  eg (y$getmatrix())
## $setmatrix() sets the object value       eg (y$setmatrix(matrix(c(1,2,3,4), 2, 2)))
## $set() sets the object value             eg (y$set(matrix(c(1,2,3,4), 2, 2)))

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## cacheSolve takes a solvable matrix as input and 
##    returns its inverse using the solve() function
## This value is cached to possibly be used again
## If the same matrix is passed to the cacheSolve 
##    function, the cached value is retrieved, otherwise 
##    the new matrix is cached and solved
##    eg (cacheSolve(y)) where y is a solvable matrix

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  thematrix <- x$get()
  m <- solve(thematrix, ...)
  x$setmatrix(m)
  m
}
