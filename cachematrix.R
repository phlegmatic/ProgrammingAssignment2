## cachesolve computes and returns invers of passed matrix and
##makecachematrix creates matrix  

## This function creates a special "matrix" object that can cache its inverse. It can
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse 

makeCacheMatrix <- function(x = numeric()) {
        cache <- NULL
        #cache is superassigne to null initially while new matrix is superassinged to x
        set <- function(y) {
                x <<- y  
                cache <<- NULL
        }
        
        #returns matrix x is accessible because of superassignement
        get <- function() {
                x
        }
        
        #can modify inverse
        setinverse <- function(inverse){
                cache <<- inverse      
        } 
        
        #retuns inverse matrix
        getinverse <- function() {
                cache
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed),
#then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) { #is i is null it means inverse is not computed
                message("We have computed inverse!!getting cached matrix!")
                return(i)
        }
        data <- x$get()
        
        #solve function calculates inverse
        i <- solve(data, ...)
        
        x$setinverse(i)
        i

}
