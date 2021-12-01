filepath <-"~/Advent of Code 2021/Day 01/P1 Input"
#filepath <-"~/Advent of Code 2021/Day 01/P1 Practice Input"

count <- 0

last1 <- NULL
last2 <- NULL
last3 <- NULL


con = file(filepath, "r")
while ( TRUE ) {
  lineInt = strtoi(readLines(con, n = 1))
  
  if (!is.null(last1) && ((last1 + last2 + last3) < (lineInt + last3 + last2))){
    count = count + 1
  }
  
  last1 = last2
  last2 = last3
  last3 = lineInt
}

close(con)

print(count)