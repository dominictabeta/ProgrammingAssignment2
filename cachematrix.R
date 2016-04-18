## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix creates a special list containing four functions
## These functions carry out the following:
##      1. set the matrix
##      2. get the matrix
##      3. set the matrix inverse
##      4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        # The set function sets the matrix. 
        ## Initially the inverse matrix is NULL as this has not been cached yet
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # The get function retrieves the matrix
        get <- function() x
        
        # This setinverse function calculates the matrix inverse and assigns it to 'inv'
        # This carries out the caching of the inverse
        setinverse <- function(solve) inv <<- solve
        
        # getinverse retrives the cached matrix inverse
        getinverse <- function() inv
        
        # The returned list of all 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes as its argument a list of functions created by the makeCacheMatrix function above
## It returns the matrix that is the inverse of 'x'
## If the inverse is already cached, the function retrieves this cached inverse and returns it
## If not, then the inverse is calculated using solve(), cached and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <<- x$getinverse()
        
        # This if statement checks whether 'inv' contains non-NULL (i.e. what should be the cached inverse)
        if(!is.null(inv)) {
                message("getting cached data")
                
                # A non-NULL inv means the inverse is already cached, so no call to solve() is made
                # The cached 'inv' is returned
                return(inv)
        }
        
        # If 'inv' is NULL, then these lines retrive the matrix from the list, call solve() on this matrix,
        # caches the inverse in 'inv' then returns 'inv'
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
