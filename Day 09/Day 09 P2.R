filepath <-"~/Advent of Code 2021/Day 09/Input"
#filepath <- "~/Advent of Code 2021/Day 09/Practice Input"

con = file(filepath, "r")

total = 1

heightmap = NULL

while (TRUE) {
  tempt = as.numeric(unlist(strsplit(readLines(
    con, n = 1, warn = FALSE
  ), split = "")))
  
  if (length(tempt) == 0) {
    break
  }
  
  if (is.null(heightmap)) {
    heightmap = matrix(tempt, nrow = 1, ncol = length(tempt))
  } else {
    heightmap = rbind(heightmap, tempt)
  }
}

rows = dim(heightmap)[1]
cols = dim(heightmap)[2]

basin_size <<- matrix(0:0, rows, cols)

run_down <- function(r, c) {
  value = heightmap[r, c]
  
  if (value == 9) {
    return()
  } else {
    up = if (r != 1)
      value - heightmap[r-1, c]
    else value * -1
    down = if (r != rows)
      value - heightmap[r+1, c]
    else value * -1
    left = if (c != 1)
      value - heightmap[r, c-1]
    else value * -1
    right = if (c != cols)
      value - heightmap[r, c+1]
    else value * -1
    
    if (up <= 0 && down <= 0 && left <= 0 && right <= 0) {
      
      basin_size[r, c] <<- basin_size[r, c] + 1
  
    } else {
      index = which.max(c(up, down, left, right))
      
      if (index == 1) {
        return(run_down(r-1, c))
      } else if (index == 2) {
        return(run_down(r+1, c))
      } else if (index == 3) {
        return(run_down(r, c-1))
      } else {
        return(run_down(r, c+1))
      }
    }
  }
}

for (r in 1:rows) {
  for (c in 1:cols) {
    run_down(r, c)
  }
}

close(con)

print(heightmap)
print(basin_size)

hold = sort(c(basin_size[basin_size > 0]), decreasing = TRUE)

print(hold[1] * hold[2] * hold[3])




