## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This is a test comment
makeCacheMatrix <- function(x = matrix()) {

    I <-NULL
    set<-function (y) {
        
        x<<-y
        I<<-NULL
        
    }
    
    get <-function() x
    setinverse<-function(inverse) I<<-inverse
    getinverse<-function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    I<-x$getinverse()
    if(!is.null(I)) {
        print("Got Cached data")
        return(I)
    }
    data<-x$get()
    
    I<-function(data){
        if (det(data)!=0) return(solve(data))
        else
            return(print("The matrix is singular"))
    }
    x$setinverse(I) 
    I
        
}

