filepath <-"~/Advent of Code 2021/Day 09/Input"
#filepath <-"~/Advent of Code 2021/Day 09/Practice Input"

con = file(filepath, "r")

total = 0

heightmap = NULL

while ( TRUE ) {
  tempt = as.numeric(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = "")))
  
  if (length(tempt) == 0) {
    break
  }
  
  if (is.null(heightmap)){
    heightmap = matrix(tempt, nrow = 1, ncol = length(tempt))
  } else {
    heightmap = rbind(heightmap, tempt)  
  }
}

rows = dim(heightmap)[1]
cols = dim(heightmap)[2]

for(r in 1:rows){
  for (c in 1:cols){
    value = heightmap[r,c]
    up = if (c != 1) (if (value < heightmap[r, c-1]) TRUE else FALSE) else TRUE
    down = if (c != cols) (if (value < heightmap[r, c+1]) TRUE else FALSE) else TRUE
    left = if (r != 1) (if (value < heightmap[r-1, c]) TRUE else FALSE) else TRUE
    right = if (r != rows) (if (value < heightmap[r+1, c]) TRUE else FALSE) else TRUE
    
    if (up && down && left && right){
      total = total + value + 1
    }
  }
}
close(con)

print(total)

