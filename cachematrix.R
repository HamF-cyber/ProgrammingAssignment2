## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             #initializing inverse as NULL
    set <- function(y) 
    {                   
      x <<- y                             #setting the value of y in the parent variable x
      inv <<- NULL                        # in case there is a new matrix, the value of inverse is set to NULL so as to store the new inverse value
    }
    get <- function() x                    
    
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     #the value of inv is retrieved where it is called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
    
  }

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("Cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)                     #the solve() function is used to calculate the inverse of a matrix
  x$setinverse(inv)
  inv
}
