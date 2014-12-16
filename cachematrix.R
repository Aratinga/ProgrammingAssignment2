## Put comments here that give an overall description of what your
## functions do
## This function pair is used to optimize the inversion of a matrix. Because matrix inversion can be expensive (time 
## and memory consuming), you would want to be able to tell if you already have the inversion of ypour matrix, and 
## in that case, to retrieve it.
## This makes use of the fact that free variables remain in scope at global level.

##  
## Write a short comment describing this function

## The first function, makeCacheMatrix, is a constructor function. It sets up a list x of functions that can be called, consisting of the following:
## 1. set - creates the matrix of data to be inverted
## 2. get - returns the data
## 3. setmatrix - executes the solve() function on the passed square matrix, and returns it to the calling environment
## 4. getmatrix - returns the matrix m 

## invoke z <- makeCacheMatrix()
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL ## m is defined as a local variable in the function

	set <- function(y) {
		x <<- y ## pass the data matrix
		m <<- NULL ## this sets the variable m in the calling environment. There is no inverted matrix yet.
	}
	get <- function() x ## return the data
	setmatrix <- function(solve) m <<- solve ## execute the solve function to invert the matriz, put in m
	getmatrix <- function() m ## return the inverted matrix
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) ## return a list of the methods of z

}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	z$set(x)
	m <- z$getmatrix()
	if (!is.null(m)) {
		message("getting cached data")
		 return(m)
		} 
		 {message("calculating inverse")
		data <- z$get()
		m <- solve(data)
		z$setmatrix(m)
	m <- z$getmatrix()	
		m
	}
	

}
