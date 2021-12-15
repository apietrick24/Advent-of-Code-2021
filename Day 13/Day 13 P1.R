filepath <-"~/Advent of Code 2021/Day 13/Input"
#filepath <- "~/Advent of Code 2021/Day 13/Practice Input"

con = file(filepath, "r")

paper <<- matrix(FALSE, nrow = 1, ncol = 1)

resize_paper <-function(new_row, new_col){
  tempt_paper = matrix(FALSE, ncol = new_col, nrow = new_row)

  for (r in 1:dim(paper)[1]){
    for(c in 1:dim(paper)[2]){
      if (paper[r,c]){
        tempt_paper[r,c] = TRUE
      }
    }
  }
  
  paper <<- tempt_paper
}

direction = NULL

completed = FALSE

current_row = 0
current_col = 0

while (TRUE) {
  if (!completed){
    tempt = c(as.numeric(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ","))))
  } else {
    tempt = c(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = " ")))
  }
  
  if (length(tempt) == 0 && is.null(direction)) {
    completed = 1
  } else if (length(tempt) == 0){
    break
  }
  
  if (!completed){
    r = tempt[1] + 1
    c = tempt[2] + 1
    
    if (r > current_row && c > current_col){
      current_row = r
      current_col = c 
      resize_paper(current_row, current_col)
    } else if (r > current_row){
      current_row = r
      resize_paper(current_row, current_col)
    } else if (c > current_col){
      current_col = c 
      resize_paper(current_row, current_col)
    }
    
    paper[r, c] = TRUE
  } else {
    if (length(tempt) != 0){
      if (is.null(direction)){
       direction = list()
     }
      direction = unlist(append(unlist(direction), tempt[3]))
    }
  }
}

close(con)

print(direction)

rotate <- function(x) t(apply(x, 2, rev))


for (i in 1:length(direction)){
  tempt = unlist(strsplit(direction[i], split = "="))
  axis = tempt[1]
  location = as.numeric(tempt[2]) + 1
  
  if (axis == "y"){
    print(length(1:(location-1)) - length((location+1):current_col))
    #paper <<- paper[,1:(location-1)] + rotate(t(paper[,(location+1):current_col]))
    if (length((location+1):current_col) != length(1:(location-1))){
      diff = length(1:(location-1)) - length((location+1):current_col)
      
      if (diff > 0){
        print("y - big on left")
        paper <<- paper[,(1 + diff):(location-1)] + rotate(t(paper[,(location+1):current_col]))
      } else {
        print("y - big on right")
        paper <<- paper[,1:(location-1)] + rotate(t(paper[,(location+1):(current_col + diff)]))
      }
    } else {
      print("y - normal diff")
      paper <<- paper[,1:(location-1)] + rotate(t(paper[,(location+1):current_col]))
    }
  } else {
    print(length(1:(location-1)) - length((location+1):current_row))
    #paper <<- paper[1:(location-1),] + t(rotate(paper[(location+1):current_row,]))
    
    if (length((location+1):current_row) != length(1:(location-1))){
      diff = length(1:(location-1)) - length((location+1):current_row)
      
      if (diff > 0){
        print("x - big on left")
        paper <<- paper[(1 + diff):(location-1),] + t(rotate(paper[(location+1):(current_row),]))
      } else {
        print("x - big on right")
        print(diff)
        print(location + 1)
        print((location+1):(current_row + diff))
        paper <<- paper[1:(location-1),] + t(rotate(paper[(location+1):(current_row + diff),]))
      }
    } else {
      print("x - normal diff")
      paper <<- paper[1:(location-1),] + t(rotate(paper[(location+1):current_row,]))
    }
  }
  
  current_row = dim(paper)[1]
  current_col = dim(paper)[2]
  
}

print(paper)

print(length(which(paper > 0)))

numbers = matrix("", nrow = current_row, ncol = current_col)

for (r in 1:current_row){
  for (c in 1:current_col){
    if (paper[r,c] > 0){
      numbers[r,c] = "X"
    } else {
      numbers[r,c] = ""
    }
  }
}

print(rotate(numbers))



