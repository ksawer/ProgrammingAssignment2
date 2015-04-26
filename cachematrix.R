## Put comments here that give an overall description of what your
## functions do

testMatrix<-matrix( #example of an invertible 4x4 matrix
c(1,2,3,4,2,3,1,2,1,1,1,-1,1,0,-2,-6),
nrow=4,
ncol=4,
byrow=TRUE)


#This function prepares set of functions 
#which are to manipulating matrix passed as an argument 
#these functions are returned by makeCacheMatrix
#function also sets an empty object which is a cache of the inverse

#makeCacheMatrix <- function(x = testMatrix) {
makeCacheMatrix <- function(x = matrix()) {
        mx <- NULL		#creating empty object
        set <- function(y) {	#setting matrix in other env. (the cache)
                x <<- y		#setting value of an initial matrix
                mx <<- NULL	#creating an empty object in other env.
        }
	get <- function() x	#getting  value of an matrix
        setinv <- function(minv) mx <<- minv #setting inverse of an matrix
        getinv <- function() mx #getting inverted matrix
        list(set = set, get = get, #Return of the Functions
             setinv = setinv,
             getinv = getinv)
}


#
cacheSolve <- function(x, ...) {#as an argument is taken return of makeCacheMatrix
	mx <- x$getinv()	#try to get the inverse of the matrix
        if(!is.null(mx)) {	#if created in makeCacheMatrix object mx is not empty...
                message("getting cached inverse")
                return(mx)	#... then return of the inverse from the cache, exit of the fonction
        }
	minv <- x$get()		#get the initial matrix
        mx <- solve(minv, ...)	#invering matrix if the cache is null
        x$setinv(mx)		#setting inversion in the cache
        mx			#Return of the Inverse of matrix passed to makeCacheMatrix
        ## Return a matrix that is the inverse of 'x'
}
