library(liqueueR)

filepath <-"~/Advent of Code 2021/Day 15/Input"
#filepath <- "~/Advent of Code 2021/Day 15/Practice Input"

con = file(filepath, "r")

chitons = NULL

while (TRUE) {
  tempt = c(as.numeric(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ""))))
  
  if (length(tempt) == 0) {
    break
  }
  
  if (is.null(chitons)){
    chitons <<- matrix(tempt, nrow = 1, ncol = length(tempt))
  } else {
    chitons <<- rbind(chitons, tempt) 
  }
}

close(con)

visited = matrix(FALSE, nrow = dim(chitons)[1], ncol = dim(chitons)[2])

min_path<-function(start_row, start_col, end_row, end_col){
  queue = PriorityQueue$new()
  queue$push(c(start_row,start_col,0), 0)

  while(queue$size() != 0){
    current_pop = queue$pop()

    row = current_pop[1]
    col = current_pop[2]
    value = current_pop[3] + chitons[row, col]
    
    #print(paste(row, "x",  col, "-", value))

    if(row == end_row && col == end_col){
      return(value - chitons[start_row, start_col])
    } 
    
    if(row != end_row && !visited[row + 1, col] && !visited[row, col]){
      queue$push(c(row + 1, col, value), (-1* value))
    }
    
    if (col != end_col  && !visited[row, col + 1] && !visited[row, col]){
      queue$push(c(row, col + 1, value), (-1* value))
    }
    
    
    visited[row, col] = TRUE
  }
}

print(min_path(1,1, dim(chitons)[1], dim(chitons)[2]))
#print(min_path(1,1, 8, 6))

