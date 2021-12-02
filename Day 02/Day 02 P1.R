filepath <-"~/Advent of Code 2021/Day 02/P1 Input"
#filepath <-"~/Advent of Code 2021/Day 02/P1 Practice Input"

con = file(filepath, "r")

forward = 0
depthChange = 0

while ( TRUE ) {
  line = readLines(con, n = 1, warn = FALSE)
  
  if (length(line) == 0) {
    break
  }
  
  lineSplit = strsplit(line, " ")[[1]]
  
  switch(lineSplit[1], 
         "forward"= (forward = forward + strtoi(lineSplit[2])), 
          "down"= (depthChange = depthChange + strtoi(lineSplit[2])),
          "up"= (depthChange = depthChange - strtoi(lineSplit[2]))
         )
}

close(con)

print(forward)
print(depthChange)
print(forward*depthChange)
