## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv_x <- NULL ##set the inverse matrix to NULL on first assignment
        setMatrix <- function(y){
                x <<- y #assigns the input matrix to the cache
                inv_x <<- NULL #sets inv_x to NULL on subsequent new matrix assignment
        }
        getMatrix <- function() x #returns the stored matrxi
        
        setMatrixInv <- function(inv_matrix) inv_x <<- inv_matrix #sets the inverse matrix (inv_x) to the input matrix (inv_matrix)
        
        getMatrixInv <- function() inv_x #returns the matrix inverse
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, setMatrixInv = setMatrixInv, getMatrixInv = getMatrixInv) #lists the names of functions in this makeCacheMatrix object
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_x <- x$getMatrixInv() #gets the inverse stored in the cache
        
        #if the inverse matrix is not NULL then returns the stored inverse matrix from the cache
        if(!is.null(inv_x)){
                message("getting cached data")
                return(inv_x)
        }
        
        matrix_x <- x$getMatrix() #gets the matrix from the cache
        inv_x <- solve(matrix_x,...) #computes the inverse of the matrix stored in the cache
        x$setMatrixInv(inv_x) #sets the computed matrix as inverse
        inv_x #returns the matrix inverse
}
