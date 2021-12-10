filepath <-"~/Advent of Code 2021/Day 10/Input"
#filepath <- "~/Advent of Code 2021/Day 10/Practice Input"

con = file(filepath, "r")

corruptions = c(0,0,0,0)

print(corruptions)

while (TRUE) {
  tempt = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = ""))
  
  if (length(tempt) == 0) {
    break
  }
  
  syntax_seeking = c()
  
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
        corruptions[1] = corruptions[1] + 1
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    } else if (current == "]"){
      if (current != syntax_seeking[1]){
        corruptions[2] = corruptions[2] + 1
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    } else if (current == "}"){
      if (current != syntax_seeking[1]){
        corruptions[3] = corruptions[3] + 1
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    } else if (current == ">"){
      if (current != syntax_seeking[1]){
        corruptions[4] = corruptions[4] + 1
        break
      } else {
        syntax_seeking = syntax_seeking[-1]
      }
    }
  }
}


print(corruptions)

print(corruptions[1]*3 + corruptions[2]*57 + corruptions[3]*1197 + corruptions[4]*25137)

close(con)




