## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list of 4 function
# Function setMatrix allows user to input the Matrix data
# Function getMatrix allows user to retrieve the Matrix data
# Function setInv allows user to store the inverse matrix
# Function getInv allows user to retrieve the inverse matrix stored

makeCacheMatrix <- function(x = matrix()) {
	Inv <- NULL	# Declare an object and initialize to NULL

	# Set the Matrix value and return the value to x.
	# This function also reset the Inv value to NULL whenever
	# a new Matrix is set.
        setMatrix <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        
	getMatrix <- function() x			# Get the Matrix values
        setInv <- function(Inverse) Inv <<- Inverse	# Set the Inverse Matrix cache value.
        getInv <- function() Inv			# Retrieve the Inverse Matrix value from cache
        
	# Assign names to the 4 functions and display the functions
	list(setMatrix = setMatrix, getMatrix = getMatrix, 
	setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
# This function check if the Inverse Matrix is cached.
# If it is, retrive the Inverse Matrix from cache, return the value, and exit the function
# If not, then calculate the Inverse Matrix, cached it, and return the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	Inv <- x$getInv()	# get the Inverse Matrix from the cache
        
	# Check if the Inverse Matrix from the cache is empty (i.e. = NULL)
	# If it is not empty, print the message and return the cached value and exit this function.
	if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
       
	# If cache is empty, function will continue here.
	data <- x$getMatrix()	# Retrieve the Matrix data
        Inv <- solve(data)	# calculate the Inverse Matrix
        x$setInv(Inv)		# store the Inverse Matrix in the cache
        Inv			# return the Inverse Matrix
}
