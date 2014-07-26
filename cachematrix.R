## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL						 ## Create blank variable to cache inverse of matrix later
	set <- function(y) {                       ## Store a new matrix
		x <<- y					 ## into x
		m <<- NULL					 ## and clear previously cached inverse
	}
	get <- function() x				 ## Return current matrix
	setinverse <- function(solve) m <<- solve  ## Cache inverse of previously unsolved matrix
	getinverse <- function() m			 ## Return previously solved inverse of matrix
	list(set = set, get = get,			 ## List of object's access methods
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then this function
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {			
	m <- x$getinverse()				 ## Look for solved inverse of matrix store in object 'x'
								 ## If no inverse has been cached yet, 'm' will equal NULL
	if(!is.null(m)) {					 ## If 'm' does not equal NULL, the inverse has been previously cached
		message("Getting cached data")	 ## Output message that data is being retrieved from cache
		return(m)					 ## Return matrix inverse and exit function
	}
	data <- x$get()					 ## If function reaches this point, inverse has not yet been calculated and cached
	m <- solve(data, ...)				 ## Solve inverse of matrix and store to 'm'
	x$setinverse(m)					 ## Cache 'm' (inverse of matrix) into object 'x' 

	m							 ## Return 'm' (inverse of matrix)	
