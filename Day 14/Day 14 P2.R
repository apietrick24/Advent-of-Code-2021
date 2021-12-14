library(hash)

filepath <-"~/Advent of Code 2021/Day 14/Input"
#filepath <- "~/Advent of Code 2021/Day 14/Practice Input"

con = file(filepath, "r")

dictionary = hash()
template = NULL

line = 0

while (TRUE) {
  if (line == 0){
    tempt = c(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = "")))
    template = tempt
  } else {
    tempt = c(unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = " -> ")))
  }
  
  if (length(tempt) == 0 && line != 1) {
    break
  }
  
  dictionary[[tempt[1]]] = tempt[2]
  
  line = line + 1
}

close(con)

print(dictionary)
print(template)

frequency = hash()
current_states = hash()

add_to_hash <- function(value, hash, volume){
  if (is.null(hash[[value]])){
    hash[[value]] = 1 * volume
  } else {
    hash[[value]] = hash[[value]] + (1 * volume)
  }
}

for(i in 1:(length(template)-1)){
  pair = paste(template[i], template[i+1], sep="")
  
  add_to_hash(pair, current_states, 1)
  add_to_hash(template[i], frequency, 1)
}

add_to_hash(template[length(template)], frequency, 1)

for(s in 1:40){
  new_states = hash()
  
  for (k in keys(current_states)){
    generate = dictionary[[k]]
    
    volume = current_states[[k]]
    
    add_to_hash(generate, frequency, volume)
    
    split_key = c(unlist(strsplit(k, split = "")))
    
    add_to_hash(paste(split_key[1], generate, sep=""), new_states, volume)
    add_to_hash(paste(generate, split_key[2], sep=""), new_states, volume)
  }
  
  current_states = new_states
}

options(scipen = 100)

print(current_states)

frequency_list = unlist(as.list(frequency))

print(frequency)


print(max(frequency_list) - min(frequency_list))



