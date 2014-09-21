## Put comments here that give an overall description of what your
## functions do
## author: xiwei yan

# First, create the function makeCacheMatrix which store the matrix and its inverse in a list
# Then, create the function CacheSolve which take the list computed above,
#      and either get the cached inverse, 
#      or if inverse does not exist or is wrong, calculate an inverse for the matrix 





## Write a short comment describing this function
# This function will take a matrix and an inverse matrix as input
# And return a list including the inputted matrix and its inputted inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse = NULL
	setmatrix = function(y=matrix()){
		matrix_x <<- y
		inverse <<- NULL
		}
	getmatrix = function() matrix_x
	setinverse = function(z=matrix()){
		inverse <<- z}
	getinverse = function() inverse
	list(setmatrix=setmatrix, getmatrix = getmatrix, setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
# This function take the list returnned by makeCacheMatrix as input
# First test whether inverse-matrix exist
# Then test whether inverse and matrix are really inverse to each otheer
CacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix = x$getmatrix()
	dimensions=nrow(matrix)
	inverse = x$getinverse()
	if(!is.null(inverse)){     
		print('Have Inverse')
		compare<- matrix %*% inverse == diag(dimensions)
		if(sum(compare)==16){   
			print('Get cached inverse')
			return(inverse)
			}
		else{
			print('Wrong Inverse')
			true_inverse = solve(matrix,diag(dimensions))
			return(true_inverse)
			}}
	else{
		print ('Do not have inverse. Calculate Inverse')
		true_inverse = solve(matrix,diag(dimensions))
	}
}
