##
##  Juan Lopez Quiles
##


##
## How to try it
##


#Lets create a basic matrix
#myMatrix <- matrix( c(1,3,2,4) , nrow = 2, ncol = 2)

#Next, we create our cacheable matrix
#myCustomMatrix <- makeCacheMatrix(myMatrix)


#The first execution dont show the comment, because the value of the inverse of the matrix is null
#cacheSolve(myCustomMatrix)

#This time the execution of the function will show up, the message "getting inverted matrix from cache" 
#Because the last execution save the value to avoid the recalculation.
#cacheSolve(myCustomMatrix)



##
## Function Section 
##

#Creates a custom matrix with the methods that can cache the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(p_inverse) inverse <<- p_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix from a cache or calculate it and store in it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting inversed matrix from cache")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
