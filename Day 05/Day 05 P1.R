filepath <-"~/Advent of Code 2021/Day 05/Input"
#filepath <-"~/Advent of Code 2021/Day 05/Practice Input"

con = file(filepath, "r")

vents = list()
size = 0

while ( TRUE ) {
  line = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = " -> "))
  
  if (length(line) == 0) {
    break
  }
  
  pair = list()
  
  for(i in 1:length(line)){
    position = as.numeric(unlist(strsplit(line[i], split = ",")))
    #print(position[1])
    
    if (position[1] > size){
      size = position[1] + 1 
    } else if (position[2] > size){
      size = position[2] + 1
    }
    
    pair = append(pair, position)
  }
  
  # Only Horizontal and Vertical Liens
  if (pair[[1]] == pair[[3]] || pair[[2]] == pair[[4]]){
    vents = append(vents, pair)
  }
}

close(con)

map = matrix(0, nrow = size, ncol = size)

num_of_vents = length(vents) / 4

for (i in 1:num_of_vents){
  x1 = vents[[1]] + 1
  y1 = vents[[2]] + 1
  x2 = vents[[3]] + 1
  y2 = vents[[4]] + 1
  
  #print(paste("Vent:", i, "-", x1, y1, "->", x2, y2))
  
  x_change = if (x2 - x1 > 1){ TRUE } else { FALSE }
  y_change = if (y2 - y1 > 1){ TRUE } else { FALSE }
  
  while (x1 != x2 || y1 != y2){
    #print(paste("Vent:", i, "-", x1, x2))
    map[x1, y1] = map[x1, y1] + 1
    
    if (x1 != x2){
      if (x_change) {
        x1 = x1 + 1
      } else {
        x1 = x1 - 1
     }
    }  
    
    if (y1 != y2){
      if (y_change){
        y1 = y1 + 1
      } else {
        y1 = y1 - 1
      }
    } 
  }
  
  map[x1, y1] = map[x1, y1] + 1
  
  vents = vents[-(1:4)]
}

print(paste("Overlaps:", length(which(map > 1))))






