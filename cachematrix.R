#This function creates a special "matrix" object that can cache its inverse.
#assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) { #assigns default value of x as empty matrix
	inv <- NULL #initially set to null
	set <- function(y) { #define set function
		x <<- y #Assign the input argument to the x object in the parent environment
		inv <<- NULL #Assign the value of NULL to the m object in the parent environment (clear any previous stored value)
	}
	get <- function() x #define get function
	setinverse <- function(solve) inv <<- solve #inverse of matrix
	getinverse <- function() inv
	
	#Return a list of methods
	list(set = set, 
		 get = get, 
		 setinverse = setinverse, 
		 getinverse = getinverse) 
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse() #return matrix inverse
	
	#return inverse if it has already been calculated
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get() #get matrix
	inv <- solve(data, ...) #cacluate matrix inverse using solve function 
	x$setinverse(inv) #set inverse to object
	inv #returns the matrix
}