filepath <-"~/Advent of Code 2021/Day 12/Input"
#filepath <- "~/Advent of Code 2021/Day 12/Practice Input"
#filepath <- "~/Advent of Code 2021/Day 12/Practice Input 2"

con = file(filepath, "r")

cave_graph = matrix(nrow = 0, ncol = 0)

print(cave_graph)

starts = list()
ends = list()
nodes = list()

add_node <- function(value){
  if (!(value %in% nodes)){
    nodes <<- append(nodes, unlist(value))
    cave_graph <<- rbind(cave_graph, rep(FALSE, length(nodes)))
    cave_graph <<- cbind(cave_graph, rep(FALSE, length(nodes)))
  }
}

location <- function(value){
  return(which(nodes == value))
}

while (TRUE) {
  tempt = c(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = "-")))
  
  if (length(tempt) == 0) {
    break
  }
  
  from = tempt[1]
  to = tempt[2]
  
  print(tempt)
  
  if (from == "start"){
    add_node(to)
    
    starts = append(starts, unlist(to))
  } else if (to == "end"){
    add_node(from)
    
    ends = append(ends, unlist(from))
  } else if (from == "end"){
    add_node(to)
    
    ends = append(ends, unlist(to))
  } else if (to == "start"){
    add_node(from)
    
    starts = append(starts, unlist(from))
  } else {
    add_node(from)
    add_node(to)
    
    cave_graph[location(from), location(to)] = TRUE
    cave_graph[location(to), location(from)] = TRUE
  }
}

close(con)

colnames(cave_graph) = nodes
rownames(cave_graph) = nodes

# https://stackoverflow.com/questions/21992491/checking-if-string-is-in-uppercase-in-r
is_upper_case <- function(s) {
  return (all(grepl("[[:upper:]]", strsplit(s, "")[[1]])))
}

visit <-function(path, new, visited){
  paths = list()
  new_path = paste(path, ",", new, sep="")
  
  visited[location(new)] = visited[location(new)] + 1
  
  neighbors = cave_graph[new, ]
  
  for(n in 1:length(neighbors)){
    cave = unlist(nodes[n])
    
    if (neighbors[n]){
      if (is_upper_case(cave) || (visited[location(cave)] < 1)){
        paths = unlist(append(paths, unlist(visit(new_path, cave, visited))))

      }
    }
  }
  
  if (new %in% ends){
    paths = unlist(append(paths, paste(new_path,",end", sep="")))
  } 
  
  return(paths)
}

all_paths = list()

print(starts)

lower_caves <- function(list_caves){
  lower_list = list()
  
  
  for(i in 1:length(list_caves)){
    if (!is_upper_case(unlist(list_caves[i]))){
      lower_list = append(lower_list, i)
    }
  }
  
  return(lower_list)
}

for(i in 1:length(starts)){
  start_location = unlist(starts[i])
  
  current_path = "Start"
  
  for (c in lower_caves(nodes)){
    visited = rep(0, length(nodes))
    
    visited[c] = -1
    
    all_paths = append(all_paths, unlist(visit(current_path, start_location, visited)))
  }
}

all_paths = unique(all_paths)

#print(all_paths)
print(length(all_paths))

