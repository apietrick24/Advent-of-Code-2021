filepath <-"~/Advent of Code 2021/Day 07/Input"
#filepath <-"~/Advent of Code 2021/Day 07/Practice Input"

con = file(filepath, "r")

fish = list()

while ( TRUE ) {
  tempt = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ","))
  
  for (i in 1:length(tempt)){
    location = strtoi(tempt[i])
    fish = unlist(append(fish, location))
  }
  
  if (length(tempt) == 0) {
    break
  }
}

close(con)

location = 0
least_fuel = NULL

for (i in min(fish):max(fish)){
  # print(paste(i, location, least_fuel))
  
  #((abs(fish - i) * (abs(fish - i) + 1)) / 2)
  
  fuel = sum(((abs(fish - i) * (abs(fish - i) + 1)) / 2))
  
  if (is.null(least_fuel) || fuel < least_fuel){
    least_fuel = fuel
    location = i
  }
}

print(location)
print(least_fuel)
