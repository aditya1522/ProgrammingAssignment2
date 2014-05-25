## This R function is able to cache potentially time-consuming computations.
# It takes the advantage of the scoping rules of the R language and how 
# they can be manipulated to preserve state inside of an R object.

###########################################################################

## Instructions for usage:

# First create a matrix mat
#  > mat <- matrix(rnorm(25), nrow = 5, ncol = 5)

# Create our special matrix ('smat')
#  > smat <- makeCacheMatrix(mat)                 

# Return the special matrix
#  > smat$get()

# Returns the inverse of the special matrix
#  > cacheSolve(smat)                            

# Call again to return the cached special inversed matrix
#  > cacheSolve(smat)                          

##########################################################################

## makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.

# The function, makeCacheMatrix creates a "matrix", by
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
# 4.get the value of the inverse

##########################################################################


makeCacheMatrix <- function(mat = matrix()) {

  # i will store the values of the cached inverse matrix (i)
  i <- NULL
  
  # 1.Set the value of the matrix
  set <- function(y) {
    mat <<- y
    i   <<- NULL
  }
  
  # 2.Get the value of the matrix
  get    <- function() mat
  
  # 3.Set the value of the inverse
  seti <- function(inverse) i <<- inverse
  
  # 4.Get the value of the inverse
  geti <- function() i
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, seti = seti, geti = geti)
}

###########################################################################

## cacheSolve: This function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix above. If the inverse has already been 
#  calculated (and the matrix has not changed), then the cachesolve should 
#  retrieve the inverse from the cache.

###########################################################################

cacheSolve <- function(mat, ...) {
  
  i <- mat$geti()
  
  # If its already calculated, then it will return the matrix
  if (!is.null(i)) {
    message("I just finished gathering data from cache. Printed it for you")
    return(i)
  }
  
  # Calculates the inverse if its not yet calculated
  dat <- mat$get()
  i   <- solve(dat, ...)
  
  # The inverse matrix is being obtained from cache
  mat$seti(i)
  
  # Returning the matrix
  i
}

########################## END #############################

## Acknowledgement everyone in discussion forums. 
#  Thank you all.