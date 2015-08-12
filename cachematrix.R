## This R functions package is my solution to the Programming Assignment 2 of a Coursera courses named : "R Programming" from JOHNS HOPKINS ("BLOOMBERG SCHOOL of PUBLIC HEALTH")
##

## Those line helped to test functions cacheSolve and makeCacheMatrix
##   source("cachematrix.R")
##   test_cacheSolve()

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	## Parameter x is a square invertible matrix

	invx <- NULL

	set <- function(y) {
		## Assign a new matrix to matrix cached
		##  <<- operator is used to assign a value to an object in an environment that is different from the current environment
		x <<- y
		
		## Clear the inverse cached matrix invx
		invx <<- NULL
	}

	get <- function() x
	
	setinv <- function(invy) {
		## Assign a new matrix to inverse cached matrix
		invx <<- invy
	}
	
	getinv <- function() invx
		
	## Return a list containing functions :
	##   set : Set the cached matrix with a square invertible matrix
	##   get : Get the cached matrix
	##   setinv : Set the inverse cached matrix
	##   getinv : Get the inverse cached matrix
	list(
		set = set,
		get = get,
		setinv = setinv,
		getinv = getinv
	)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
##    If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	## Parameter x is an element created by makeCacheMatrix function

	## Is the inverse matrix already been cached ?
	inv_data <- x$getinv()
	if(!is.null(inv_data)) {
		message("Getting cached data")
		## Yes, so return inverse cached matrix
		return(inv_data)
	}
	
	## The inverse matrix has to be calculated from cached matrix...
	data <- x$get()
	inv_data <- solve(data, ...)
	
	## ...and cached
	x$setinv(inv_data)

	## Return a matrix that is the inverse of 'x'
	return(inv_data)
}

test_cacheSolve <- function() {
	x = makeCacheMatrix(matrix(rnorm(1000000), nrow=1000, ncol=1000))
	
	print("First Iteration :")
	#Determines how much real and CPU time (in seconds) the currently running R process has already taken
	#ptm <- proc.time()
	
	stm <- Sys.time()
	inv_matrix <- cacheSolve(x)
	#print(proc.time() - ptm)
	print(Sys.time() - stm)
	

	print("Second Iteration :")
	#ptm <- proc.time()
	stm <- Sys.time()
	inv_matrix <- cacheSolve(x)
	#print(proc.time() - ptm)
	print(Sys.time() - stm)
	
	print("Third Iteration :")
	#ptm <- proc.time()
	stm <- Sys.time()
	inv_matrix <- cacheSolve(x)
	#print(proc.time() - ptm)
	print(Sys.time() - stm)
	
}