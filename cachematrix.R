## Helvio Markman - Practical R - Programming Assignment 2: Lexical Scoping 

## the first function 'makeCacheMatrix' prepare a object from a squared matrix to keep
## the inversion of matrix and four functions to get and set the original matrix and to 
## set and get the inverted matrix
## before run create the cachematrix
## example: 
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8)
## ph8 = makeCacheMatrix(h8)
## then calc the inverse
## sh8 = cacheSolve(ph8)
## if there is not preview calculated inverted matrix it will calc and store in ph8
## if it were calculed before will bring from cache and display the message "getting cached data"

makeCacheMatrix <- function(x = matrix()) {
  invetion <- NULL
  get <- function(){
    x
  }
  set <- function(new_matrix){
    x <<- new_matrix
  }
  setinv <- function(inv){
    invetion <<- inv
  }
  getinv <- function(){
    invetion
  }
  list(set = set
       , get = get
       , setinv = setinv
       , getinv = getinv
       )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  mt <- x$get()

  if(is.null(mt)){
    message('this matrix is missed')
    return()
  }

  if(nrow(mt)!=ncol(mt)){
    message('this matrix is not inversable')
    return()
  }

  mti <- x$getinv()

  if(is.null(mti)){
    x$setinv(solve(mt,...))
  }
  else{
    message("getting cached data")
    x$getinv()
  }

}
