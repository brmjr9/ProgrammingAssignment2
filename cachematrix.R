## Bruce Montgomery 2/17/2015 brmjr@computer.org

## These functions are based on the example code from ProgrammingAssignment2
## in the Coursera R Programming class.  The assignment is described on GitHub
## from the branch rdpeng/ProgrammingAssignment2.

## These functions make a special matrix object, created by makeCacheMatrix,
## that caches the result of the first call to cacheSolve.  In this way, the
## solve function is only calculated once, subsequent calls to cacheSolve
## retrieve the cached solve result.  The function uses the <<- operator
## that allows assignment to objects in other environments.

## Example of using the functions:
## > m = matrix(c(4,3,3,2),nrow=2,ncol=2)
## > m
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > cm = makeCacheMatrix(m)
## > cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(cm)
## getting cached data
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## makeCacheMatrix - this function takes an invertible matrix as an input
## and creates an object that maintains both the matrix and the last solution
## to solve (calculating the matrix inverse).  Note that the matrix provided
## must be invertible.
makeCacheMatrix <- function(x = matrix()) {
	## Create an object that holds both a provided matrix and its inverse.
	inv <- NULL
    set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) inv <<- solve
	getinverse <- function() inv
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}

## cacheSolve - this function takes an object created by makeCacheMatrix
## as an input, and returns the inverse of that matrix using the solve()
## function.  The first time this function is called, the inverse is calculated
## and cached.  Subsequent calls return the cached inverse value.  Note
## that the matrix provided must be invertible.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
