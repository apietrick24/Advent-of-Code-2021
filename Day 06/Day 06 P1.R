filepath <-"~/Advent of Code 2021/Day 06/Input"
#filepath <-"~/Advent of Code 2021/Day 06/Practice Input"

con = file(filepath, "r")

fish = list()

for (i in 1:9){
  fish = append(fish, 0)
}

fish = unlist(fish)


while ( TRUE ) {
  tempt = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ","))
  
  for (i in 1:length(tempt)){
    location = strtoi(tempt[i]) + 1
    fish[location] = fish[location] + 1
  }

  if (length(tempt) == 0) {
    break
  }
}

close(con)

for (t in 1:80){
  babies = fish[1]
  
  fish = append(fish[-1], babies)
  fish[7] = fish[7] + babies
}

print(fish)
print(sum(fish))