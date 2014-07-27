## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix contains a group of functions as objects within it.
#These sub functions are used to set global variables which are in the gloabl environment
#sub functions will set the values of the matrix, its inverse  and will obtain those values if called.
makeCacheMatrix <- function(x = matrix()) {
    
#Initialise a NULL value to I, which would be used to store the Inverse of the matrix
    I <-NULL
#Create a function set to assign the matrix y which is passed as an argument to a global varible X
    set<-function (y) {
#Assign the matrix to a global variable x
#Assign the global variable I which will be used to store the inverse of the matrix a null value.
        x<<-y
        I<<-NULL
        
    }
#create a function to obtain the value of x if it  already exists
    get <-function() x
#The function setinverse assigns the values of 'inverse' to the global variable I
    setinverse<-function(inverse) I<<-inverse
#The function getinverse obtains the values of the global variable I
    getinverse<-function() I
#coalesce all functions and their return values into one list
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
    
}


## Write a short comment describing this function
#cacheSolve function uses the global variables set in the makeCacheMatrix function to obtain existing 
#inverse of a matrix, if not, it will calculate the detrminanet of the matrix and if the determinant is not zero, 
#it would calculate the inverse of the matrix and sets it to the global variable using the setinverse subfunction described above
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
#If inverse of a matrix already exists then assign it to the local variable I 
    I<-x$getinverse()
#If the Local variable I holding the inverse of the matrix x is not null then notify that we used the cached data
    if(!is.null(I)) {
        print("Got Cached data")
        return(I)
    }
    
#obtain the matrix x using the get function
    data<-x$get()
    
#obtain the inverse of the matrix which is passed as an argument called 'data'
#a logical condition is put in to check if the determinant of the matrix is zero
#if the determinant of the matrix is zero then print a message, calling out the singularity of the matrix
#if not calculate the inverse of the matrix using the built in solve function
    I<-function(data){
        if (det(data)!=0) return(solve(data))
        else
        return(print("The matrix is singular"))
    }
    
#set the Global variable I as the inverse of the matrix
    x$setinverse(I) 
# Return the Inverse of the matrix
    I
    
}

