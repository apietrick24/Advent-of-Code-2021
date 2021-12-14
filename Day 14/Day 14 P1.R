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

count_letter <-function(left, right, deep){
  if (!(deep <= 0)){
    new_letter = dictionary[[paste(left, right, sep="")]]
    
    if (is.null(frequency[[new_letter]])){
      frequency[[new_letter]] = 0
    } else {
      frequency[[new_letter]] = frequency[[new_letter]] + 1
    }
    
    count_letter(left, new_letter, deep - 1)
    count_letter(new_letter, right, deep - 1)
  }
}

for(i in 1:(length(template) - 1)){
  print(i)
  current_left = template[i]
  current_right = template[i+1]
  
  if (is.null(frequency[[current_left]])){
    frequency[[current_left]] = 1
  } else {
    frequency[[current_left]] = frequency[[current_left]] + 1
  }
  
  count_letter(current_left, current_right, 10)
}

frequency[[template[length(template)]]] = frequency[[template[length(template)]]] + 1

frequency_list = unlist(as.list(frequency))

print(frequency)

print(max(frequency_list) - min(frequency_list))



