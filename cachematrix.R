## R Progrmaing Assignments 2
## The following functions allows calculating inverse of matrix, and caching the results for later use 
## usage: consider a given matrix m:
## cm <- makeCacheMatrix(m)
## res <- cacheSolve(cm)


## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Return a matrix that is the inverse of 'x'
## uses internal caching of comptuations
cacheSolve <- function(x, ...) 
{
		#get chachecd inverse, if its not null return it
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		#otherwise compute the inverse, store to cache and return
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}		
