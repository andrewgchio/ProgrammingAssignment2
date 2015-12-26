## Returns a matrix object that can cache the 
## inverse of the matrix. This object is contained
## in a list of four functions that get and set the
## matrix, and get and set the inverse of the matrix
## Note: This function does not actually solve for a
## the inverse of a matrix, it only holds the data
makeCacheMatrix <- function(x = matrix()) {
	inversed_matrix <- NULL
	# Sets the value of the matrix
	set_matrix <- function(y) {
		x <<- y
		inversed_matrix <<- NULL
	}
	# Returns the matrix currently stored in the cache
	get_matrix <- function() {
		x
	}
	# Sets the inversed matrix into the cache
	set_inverse_matrix <- function(mat) {
		inversed_matrix <<- mat
	}

	# Returns the inversed matrix in the cache
	get_inverse_matrix <- function() {
		inversed_matrix
	}
	list( set_matrix = set_matrix, 
		get_matrix = get_matrix, 
		set_inverse_matrix = set_inverse_matrix,
		get__inverse_matrix = get_inverse_matrix)	
}

## Given a matrix, solve for the inverse and return it. 
## If the matrix has a cached inverse already, return 
## the cached matrix instead. 
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inversed <- x$get_inverse_matrix()
	if(!is.null(inversed)) {
		return (inversed)
	}
	mat <- x$get_matrix()
	inversed_matrix <- solve(mat)
	x$set_inverse_matrix(inversed_matrix)
	inversed_matrix
}