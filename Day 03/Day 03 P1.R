filepath <-"~/Advent of Code 2021/Day 03/Input"
#filepath <-"~/Advent of Code 2021/Day 03/Practice Input"

con = file(filepath, "r")

positiveBits = NULL

while ( TRUE ) {
  line = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ""))
  
  if (length(line) == 0) {
    break
  }
  
  if (is.null(positiveBits)){
    positiveBits = array(seq(0, 0, length.out=length(line)))
  }
  
  for (i in 1:length(line)) {
    if (line[i] == "1"){
      positiveBits[i] = positiveBits[i] + 1
    } else {
      positiveBits[i] = positiveBits[i] - 1
    }
  }
}

print(positiveBits)

gamma = ""
epsilon = ""

for (i in 1:length(positiveBits)){
  if (positiveBits[i] < 0){
    gamma = paste(gamma, "0", sep="")
    epsilon = paste(epsilon, "1", sep="")
  } else {
    gamma = paste(gamma, "1", sep="")
    epsilon = paste(epsilon, "0", sep="")
  }
}

print(paste("Gamma:", gamma))
print(paste("Epsilon:", epsilon))

print(strtoi(gamma, base = 2) * strtoi(epsilon, base = 2))

close(con)