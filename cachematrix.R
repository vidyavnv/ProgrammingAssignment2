## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		
		## Initialize inverse
		invs <- NULL
		
		## Set the matrix
		set <- function(matrix) {
			x <<- matrix
			invs <- NULL
		}
		
		## Get the matrix
		get <- function() {
			x
		}
		
		## Set Inverse
	    setInverse <- function(inverse) {
           invs <<- inverse
        }

		##Get inverse
		getInverse <- function() {
			invs
		}
		
		## Returns list of methods
		list(set = set, get=get,
		     setInverse=setInverse,
		     getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Return inverse if already set
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }   
        
        ## Get matrix
        data <- x$get()
        
        ## Calculate inverse
        m <- solve(data) %*% data
        
        ##Set inverse
        x$setInverse(m)
        
        ##Return matrix
        m
}
