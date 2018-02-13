#Lim, problem set 3
#sorting hat and door
################################################################
#sortinghat
#s3

#1
student<-function(name){
  courage<-sample(c(1:100),1)
  ambition<-sample(c(1:100),1)
  intelligence<-sample(c(1:100),1)
  effort<-sample(c(1:100),1)
  student <- list(name=name, courage=courage, ambition=ambition, intelligence=intelligence, effort=effort)
  class(student) <- "student"

  return(student)
}

Lim<-student("Lim") #giving a student stats


sortinghat <- function (x, ...) {  #generic function
  UseMethod("sortinghat", x)
}

#2
sortinghat.student<-function(x){ #function for sorting
  stat<-student(x)
  stat<-as.numeric(c(stat$courage,stat$ambition,stat$intelligence,stat$effort))
  g<-c(1,2,3,4)
  s<-c(2,3,4,1)
  r<-c(3,4,2,1)
  h<-c(4,2,3,1)
  mat<-rbind(g,s,r,h)
  result<-t(mat)%*%(stat)
  if(result[1]==max(result)){
    return("GRYFFINDOR!")
  }
  if(result[2]==max(result)){
    return("SLYTHERIN!")
  }
  if(result[3]==max(result)){
    return("RAVENCLAW!")
  }
  if(result[4]==max(result)){
    return("HUFFLEPUFF!")
  }
}

sortinghat(Lim)


sortinghat.student<-function(x){  #sorting and give a second class
  stat<-student(x)
  stat<-as.numeric(c(stat$courage,stat$ambition,stat$intelligence,stat$effort))
  g<-c(1,2,3,4)
  s<-c(2,3,4,1)
  r<-c(3,4,2,1)
  h<-c(4,2,3,1)
  mat<-rbind(g,s,r,h)
  result<-t(mat)%*%(stat)
  if(result[1]==max(result)){
    class(x)<- c(class(x), "GRYFFINDOR")
    return(x)
  }
  if(result[2]==max(result)){
    class(x)<- c(class(x), "SLYTHERIN")
    return(x)
  }
  if(result[3]==max(result)){
    class(x)<- c(class(x), "RAVENCLAW")
    return(x)
  }
  if(result[4]==max(result)){
    class(x)<- c(class(x), "HUFFLEPUFF")
    return(x)
  }
}


Lim<-sortinghat(Lim)

#3
Gryffindor_Tower<-new.env()  #new environment
Black_Lake<-new.env()
Ravenclaw_Tower<-new.env()
Basement<-new.env()

curfew<-function(x,...){  #generic function
  UseMethod("curfew", x)
}

curfew.student<-function(x){  #assign evironment by schools
  name<-x$name
  if(class(x)[2]=="GRYFFINDOR"){
    assign(name, "", envir = Gryffindor_Tower)}
  if(class(x)[2]=="SLYTHERIN"){
    assign(name, "", envir = Black_Lake)}
  if(class(x)[2]=="RAVENCLAW"){
    assign(name, "", envir = Ravenclaw_Tower)}
  if(class(x)[2]=="HUFFLEPUFF"){
    assign(name, "", envir = Basement)}
}

curfew(Lim) 

ls(Gryffindor_Tower)  #check where I am 
ls(Black_Lake)
ls(Ravenclaw_Tower)
ls(Basement)

###############################
#Door
#s3
door<-sample(1:3,1)    #make class door
class(door) <- "door"


PlayGame <- function (x, ...) { #generic function
  UseMethod("PlayGame", x)
}

PlayGame.door<-function(x){ #play game function
  x<-sample(1:3,1)
  if(x==door){
    
    print("Congrats!")
  } else { print("Loser!")}
}

PlayGame(door)


#s4
door<-sample(1:3,1)  #give values for door
door

setClass(Class="door",    #set class for s4
         representation=representation(door = "numeric"),
         prototype=prototype(door=numeric()))
new("door")
new("door", door=1)


setValidity("door", function(object){  #validity test for integer, door
  if(is.integer(door)==F){return("@door is not a valid value")}
}
)

setMethod("initialize", "door", function(.Object, ...) { #set method for door and play game
  value = callNextMethod()
  validObject(value)
  return(value)
})


new("door", door=1)    #check

setGeneric("PlayGame",     #set generic function for s4
           function(object="door"){
             standardGeneric("PlayGame")
           })

setMethod("PlayGame", "door",  #set method for actual function with s4
          function(object){
            object<-sample(1:3,1)
            class(door) <- "door"  #?? Do need to do this?
            if(object==door){
              
              print("Congrats!")
            } else { print("Loser!")}
          })

myObject<-new("door", door=door) #check
myObject
PlayGame(myObject)


