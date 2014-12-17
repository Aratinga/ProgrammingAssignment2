## These functions are used to optimize the inversion of a matrix. Because matrix inversion can be expensive (time 
## and memory consuming), you would want to be able to tell if you already have the inversion of ypour matrix, and 
## in that case, to retrieve it.
## This makes use of the fact that free variables remain in scope at global level.

##  
## The first function, makeCacheMatrix, is a constructor function. It sets up a list of functions that can be called, consisting of the following:
## 1. set - stores the matrix of data to be inverted, and initialises the free variable m
## 2. get - returns the stored data
## 3. setmatrix - executes the solve() function on the passed square matrix, and puts the result in m
## 4. getmatrix - returns the matrix m 
##
## These functions are loaded into the list z by using the following line
##  z <- makeCacheMatrix()
##  
makeCacheMatrix <- function(x = matrix()) {
	set <- function(y) {
		x <<- y    ## pass the data matrix
		m <<- NULL ## this sets the variable m in the global environment. There is no inverted matrix yet.
	}
	get <- function() x ## return the data
	setmatrix <- function(x) m <<- solve(x) ## execute the solve function to invert the matriz, put in m
	getmatrix <- function() m ## return the inverted matrix
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) ## return a list of the methods of z
}

## Before using the cacheSolve function, be sure to load the matrix you want to invert with the following statement:
## z$set(mydata)
## This will initialise the global variable m and store the data in x. If you want a different matrix inverted, 
## use this set function again.
##
## The cacheSolve function uses the functions defined in makeCacheMatrix to decide 
## whether to calculate the inverse or use the one previously calculated. 
## In either case the inverted matrix is returned. The messages are there just to illustrate 
## what is happening. That is also why I used different local variables.
##  

cacheSolve <- function() {
	## Return a matrix that is the inverse of 'x', the matrix set in z$set(mydata)

	r <- z$getmatrix()                     ## if you have the matrix already, you are done
	if (!is.null(r)) {                     ## the global variable m is initialised to NULL
		message("getting cached data")     ## whenever you send another matrix to be inverted
		return(r)
	} else {
		message("calculating inverse")
		data <- z$get()                    ## retrieve the data you stored with z$set
		message("got data")
		z$setmatrix(data)                  ## solve to get the inverted matrix, and store it
		q <- z$getmatrix()                 ## retrieve the inverted matrix 
		q
	}
}
