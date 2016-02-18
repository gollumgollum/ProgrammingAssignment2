## Uygar ER
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix function has four functions,
## Two of them sets or gets original matrix.
## And the other two, sets or gets inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    ## NULL is initialize state of cachedInverse 
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseData ) {
	cachedInverse <<- inverseData
    }
    getinverse <- function() cachedInverse 

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Firstly cacheSolve function checks whether the inverse matrix has been created or not. 
## If the inverse matrix was not created, it gets the original data, calculates inverse and returns.
## If the inserse matrix was created before, it just returns the cached matrix.

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    inverseMatrix <- x$getinverse()
    if(is.null(inverseMatrix )) {
        message("First we are calculating inverse matrix!..")
        matrixData <- x$get()
	## Calculate inverse matrix and set it.
        inverseMatrix <- solve(matrixData )
        x$setinverse(inverseMatrix )
        return(inverseMatrix)
    }
    else
    {
	message("Now we are getting cached data for inverse matrix...")
        return(inverseMatrix)
    }
}

## In order to run the code
## Copy cachematrix.R file to your working directory
## Load source : 
## > source("cachematrix.R")
## Create sample matrix : 
## > x <- matrix(rnorm(4*4), 4)
## Run makeCacheMatrix function :
## > m = makeCacheMatrix(x)
## Get initial matrix :
## > m$get()
## Run cacheSolve function for calculate inverse
## > cacheSolve(m)
## Run cacheSolve function again to get cached inverse
## > cacheSolve(m)
