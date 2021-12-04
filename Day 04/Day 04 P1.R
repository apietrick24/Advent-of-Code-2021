filepath <-"~/Advent of Code 2021/Day 04/Input"
#filepath <-"~/Advent of Code 2021/Day 04/Practice Input"

bingo <-function(card){
  for (i in 1:5){
    isBingoR = TRUE
    isBingoC = TRUE
    
    for(k in 1:5){
      if (card[i,k] != -1){
        isBingoR = FALSE
      }
      
      if (card[k,i] != -1){
        isBingoC = FALSE
      }
    }
    
    if (isBingoR ||isBingoC){
      return(sum(card) + (sum(card[card == -1]) * -1 ))
    } 
  }
  
  return(NULL)
}

callNumber <-function(cards, calls){
  while (length(calls) != 0){
    current = calls[1]
    calls = calls[-1]
    
    #print(paste("Curretn Call:", current))
    
    for(i in 1:length(cards)){
      currentCard = cards[[i]]
      
      currentCard[currentCard == current] <- -1
      
      cards[[i]] = currentCard
      
      sumValue = bingo(currentCard)
      
      if(!is.null(sumValue)){
        return(sumValue * strtoi(current))
      }
    }
  }
}

con = file(filepath, "r")

fileLength = length(readLines(file(filepath, "r"), warn = FALSE))

calls = NULL
allCards = list()

for (i in 0:((fileLength - 1) / 6) + 1){
  if (i == 1){
    calls = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ","))
  } else {
    readLines(con, n = 1, warn = FALSE)
    line = unlist(strsplit(readLines(con, n = 5, warn = FALSE), split = " "))

    card = c()
    
    for(k in 1:length(line)){
      if (line[k] != ""){
        card = append(card, strtoi(line[k]))
      }
    }
    
    allCards[[i - 1]] = matrix(card, nrow = 5)
  }
  
}
close(con)

print(paste("Final Score:", callNumber(allCards, calls)))


