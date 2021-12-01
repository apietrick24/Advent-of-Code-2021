filepath <-"~/Advent of Code 2021/Day 01/P1 Input"
#filepath <-"~/Advent of Code 2021/Day 01/P1 Practice Input"

last <- NULL
count <- 0

con = file(filepath, "r")
  while ( TRUE ) {
    lineInt = strtoi(readLines(con, n = 1))
    
    if (!is.null(last) && lineInt > last){
     count = count + 1
   }
    
    last = lineInt
  }
close(con)

print(count)
