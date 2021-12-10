filepath <-"~/Advent of Code 2021/Day 10/Input"
#filepath <- "~/Advent of Code 2021/Day 10/Practice Input"

con = file(filepath, "r")

completion_scores = c()

while (TRUE) {
  tempt = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ""))
  
  if (length(tempt) == 0) {
    break
  }
  
  syntax_seeking = c()
  
  corrupted = FALSE
  
  for (i in 1:length(tempt)){
    current = tempt[i]
    
    if (current == "("){
      syntax_seeking = unlist(c(")", syntax_seeking))
    }  else if (current == "["){
      syntax_seeking = unlist(c("]", syntax_seeking))
    } else if (current == "{"){
      syntax_seeking = unlist(c("}", syntax_seeking))
    } else if (current == "<"){
      syntax_seeking = unlist(c(">", syntax_seeking))
    } else if (current == ")"){
      if (current != syntax_seeking[1]){
        corrupted = TRUE
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    } else if (current == "]"){
      if (current != syntax_seeking[1]){
        corrupted = TRUE
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    } else if (current == "}"){
      if (current != syntax_seeking[1]){
        corrupted = TRUE
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    } else if (current == ">"){
      if (current != syntax_seeking[1]){
        corrupted = TRUE
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    }
  }
  
  score = 0
  if (!corrupted){
  for (i in 1:length(syntax_seeking)){
    current = syntax_seeking[i]
    
    value = 0
    
    if (current == ")"){
      value = 1
    }  else if (current == "]"){
      value = 2
    } else if (current == "}"){
      value = 3
    } else if (current == ">"){
      value = 4
    }
    score = score*5 + value
  }
  
  completion_scores = c(unlist(completion_scores), score)
}
}

print(completion_scores)

print(sort(completion_scores))

print(sort(completion_scores)[length(completion_scores)/2 + 1])

close(con)




