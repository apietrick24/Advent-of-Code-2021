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

expand<-function(graph){
  expand_graph = matrix(0, nrow = dim(graph)[1]*5, ncol = dim(graph)[2]*5)
  
  for(r in 1:5){
    rows = 1 + ((r-1) * dim(graph)[1]) : ((r * dim(graph)[1]) - 1)
    
    for (c in 1:5){
      cols = 1 + ((c-1) * dim(graph)[2]) : ((c * dim(graph)[2]) - 1)
      
      expand_graph[rows, cols] = (graph + ((r + c) - 2)) %% 9
    }
  }
  
  expand_graph[expand_graph == 0] <- 9
  
  return(expand_graph)
}

test = matrix(8, nrow = 1, ncol = 1)

print(expand(test))

min_path<-function(graph, start_row, start_col, end_row, end_col){
  print(paste(start_row, "x", start_col, "->", end_row, "x",end_col))
  
  queue = PriorityQueue$new()
  queue$push(c(start_row,start_col,(-1 * graph[start_row, start_col])), 0)
  
  visited = matrix(FALSE, nrow = dim(graph)[1], ncol = dim(graph)[2])
  
  while(queue$size() != 0){
    current_pop = queue$pop()
    
    row = current_pop[1]
    col = current_pop[2]
    value = current_pop[3] + graph[row, col]
    
    if(row == end_row && col == end_col){
      return(value)
    } 
    
    if(row != end_row && !visited[row + 1, col] && !visited[row, col]){
      queue$push(c(row + 1, col, value), (-1* value))
    }
    
    if (col != end_col && !visited[row, col + 1] && !visited[row, col]){
      queue$push(c(row, col + 1, value), (-1* value))
    }
    
    visited[row, col] = TRUE
  }
}

all_chitons = expand(chitons)

print(min_path(all_chitons, 1,1, dim(all_chitons)[1], dim(all_chitons)[2]))

