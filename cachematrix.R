## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
   invers <- NULL
  set <- function(y)
  {
     x <<- y
     invers <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) invers <<- inverse
    getinverse <- function() invers
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x)
{
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getinvers()
        if(!is.null(inv))
        {
          message("getting cached result")
          return(invers)
        }
        data <- x$get()
        invers <- solve(data)
        x$setinverse(invers)
        invers
          }
}
