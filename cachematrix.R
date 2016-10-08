

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




## mat <- matrix(rnorm(16),4,4)
## mat1 <- makeCacheMatrix(mat)
## cacheSolve(mat1)
## [,1]       [,2]       [,3]       [,4]
## [1,] -0.1653269  0.2592203  0.6176218 -0.7520955
## [2,]  0.2828334 -0.1853499  0.4511382  0.2094365
## [3,]  0.1434840  1.0413868 -0.3550853 -0.3261154
## [4,]  0.1793583 -0.4252171 -0.4371493 -0.1749830
