## The makeCacheMatrix function provides the parameters for defining the initial matrix and calling
## additional functions on the cached data. CacheSolve computes, caches, and returns matrix
## inverse 

## This function creates a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      # Declare function and set matrix parameters
  m <- NULL                                      # 
  set <- function(y) {                           # Sets the function for modifying the existing matrix
    x <<- y                                      # Once redefined, information is stored (x) in containing env.   
    m <<- NULL                                   #   (outside local environment)
  }  
  get <- function() x                            # function to retrieve matrix defined in this function
  setinverse <- function(inverse) m <<- inverse  # 
  getinverse <- function() m                     # function to retrieve matrix inverse. If inverse has not
  list(set = set, get = get,                     #    been calculated/stored from "cacheSolve", returns "NULL" 
       setinverse = setinverse,
       getinverse = getinverse)
}

##Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {                # Declare function for computing matrix inverse
  m <- x$getinverse()                           # returns matrix inverse when $getinverse() is called on variable
  if(!is.null(m)) {                             #   however, if data has already been cached
    message("getting cached data")              #   function will not recompute, will return already
    return(m)                                   #   stored data
  }
  data <- x$get()                               # returns the original matrix when $get() is called on variable
  m <- solve(data, ...)                         # computes and stores matrix inverse
  x$setinverse(m)                               # 
  m
}
