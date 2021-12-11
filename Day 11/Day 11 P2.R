filepath <-"~/Advent of Code 2021/Day 11/Input"
#filepath <- "~/Advent of Code 2021/Day 11/Practice Input"

con = file(filepath, "r")

creatures = NULL

row = 1
sync = 0

while (TRUE) {
  tempt = c(as.numeric(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ""))))
  
  if (length(tempt) == 0) {
    break
  }
  
  if (is.null(creatures)){
    creatures <<- matrix(tempt, nrow = 1, ncol = 10)
  } else {
    creatures <<- rbind(creatures, tempt)
  }
  
}
close(con)

withinBounds <-function(row, col){
  if (row <= 10 && row >= 1 && col <= 10 && col >= 1){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

update <-function(row, col){
  for (c in -1:1){
    for (r in -1:1){
      if (withinBounds(r + row, c + col)){
        creatures[r + row, c + col] <<- creatures[r + row, c + col] + 1
      }
    }
  }
}

total_flashes = 0

s = 1
while(TRUE){
  print(paste("Step: ", s))
  creatures = creatures + 1
  
  flashed = matrix(FALSE, 10, 10)
  
  flash = c(which(creatures > 9))
  
  i = 1
  while (i <= length(flash)){
    col_index = as.integer(floor(flash[i] / 10)) 
    row_index = as.integer(floor(flash[i] %% 10))
    
    if (row_index == 0){
      row_index = 10
    } else {
      col_index = col_index + 1
    }
    
    if (col_index == 11){
      col_index = 10
    }
    
    #print(paste(row_index, " and then ", col_index))
    
    if (!flashed[row_index, col_index]){
      update(row_index, col_index)
      flashed[row_index, col_index] = TRUE
      
      new_flash = c(which(creatures > 9))
      
      new_flash <- new_flash[! new_flash %in% flash]
      
      flash = c(flash, new_flash)
    }
    
    i = i + 1
  }
  
  if (length(which(creatures > 9)) == 100){
    sync = s
    break
  }
  
  creatures[creatures > 9] = 0
  
  s = s + 1
}

print(sync)
print(creatures)




