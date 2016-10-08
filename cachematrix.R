

##Calculating inverse of a matrix is costly hence instead of finding inverse repeatedly


## makeCacheMatrix is a function which creates a special "matrix" object that can
## cache its inverse for the input (which is an invertible square matrix)

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



## cacheSolve is a function which computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache


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
