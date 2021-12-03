filepath <-"~/Advent of Code 2021/Day 03/Input"
#filepath <-"~/Advent of Code 2021/Day 03/Practice Input"

readData <-function(passed, oxygen, index){
  input <- passed

  if (length(input) == 0 || length(input[[1]]) < index){
    return("")
  } else if (length(input) == 1){
    return(paste(input[[1]][index], readData(input, oxygen, index + 1), sep=""))
  }
  
  count = 0
  symbol = ""
  
  remove1 = c()
  remove0 = c()
  
  for(i in 1:length(input)){
    if (input[[i]][index] == "1"){
      count = count + 1
      remove1 = append(remove1, i)
    } else {
      count = count - 1
      remove0 = append(remove0, i)
    }
  }
  
  if (count < 0){
    if (oxygen){ symbol = "0" } else { symbol = "1" }
  } else if (count > 0){
    if (oxygen){ symbol = "1" } else {symbol = "0" }
  } else {
    if (oxygen){ symbol = "1" } else { symbol = "0" }
  }
  
  remove = NULL
  
  if (symbol == "0"){
    remove = remove1
  } else {
    remove = remove0
  }
  
  if (!is.null(remove)){
    input = input [-remove]
  }
  
  return(paste(symbol, readData(input, oxygen, index + 1), sep=""))
}

con = file(filepath, "r")

numbers = list()

while ( TRUE ) {
  line = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ""))

  if (length(line) == 0) {
    break
  }

  numbers = append(numbers, list(line))
}

close(con)

oxygen = readData(numbers, TRUE, 1)
CO2 = readData(numbers, FALSE, 1)

print(paste("Oxygen Generator Rating:", oxygen))
print(paste("CO2 Scrubber Rating:", CO2))

print(strtoi(oxygen, base = 2) * strtoi(CO2, base = 2))
