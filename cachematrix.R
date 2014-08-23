makeCacheMatrix <- function(cachedMatrix) {
	# Creates a special cacheable "matrix" object that can also cache its 
	#   inverse.
	# Creates getter/setter methods for base and inverted matrix objects
	#
	# Args:
	#   cachedMatrix: base matrix to cache
	#
	# Returns:
	#    list of getter/setter methods for base/inverted matrices
	#
	#    setMatrix:      no return value
	#    getMatrix:      cached base matrix
	#    setInverted:    no return value
	#    getInverted:    cached inverted matrix

	if(!is.matrix(cachedMatrix)) stop("cachedMatrix must be a matrix")
        cachedInverted <- NULL

        setMatrix <- function(argMatrix) {
		if(!is.matrix(argMatrix)) stop("argMatrix must be a matrix")
                cachedMatrix <<- argMatrix
                cachedInverted <<- NULL
        }

        getMatrix <- function() 
	{
		cachedMatrix 
	}

        setInverted <- function(argInverted) 
	{
		if(!is.matrix(argInverted)) stop("argInverted must be a matrix")
		cachedInverted <<- argInverted
	}

        getInverted <- function() 
	{
		cachedInverted 
	}

        list(setMatrix = setMatrix, 
		getMatrix = getMatrix,
             	setInverted = setInverted,
             	getInverted = getInverted)
}

cacheSolve  <- function(specialMatrix, ...) {
	# Computes the inverse of the special "matrix" returned by 
	#   makeCacheMatrix. If the inverse has already been calculated 
	#   (and the matrix has not changed), then cacheSolve should 
	#   retrieve the inverse from the cache.
	#
	# Args:
	#   specialMatrix: matrix object that has cached base matrix, inverted 
	#       matrix with corresponding getter/setter methods
	#
	# Returns:
	#	inverted matrix	

        cachedInverted <- specialMatrix$getInverted()
        if(!is.null(cachedInverted)) {
                message("getting cached matrix")
                return(cachedInverted)
        }

        cachedMatrix <- specialMatrix$getMatrix()
        cachedInverted <- solve(cachedMatrix)
        specialMatrix$setInverted(cachedMatrix)
        cachedInverted
}