filepath <-"~/Advent of Code 2021/Day 08/Input"
#filepath <-"~/Advent of Code 2021/Day 08/Practice Input"
#filepath <-"~/Advent of Code 2021/Day 08/Practice Practice Input"

con = file(filepath, "r")

number_shared <-function(long, short){
  #print(paste("Finding", toString(short), "in", toString(long)))

  long = unlist(strsplit(toString(long), split = ""))
  short = unlist(strsplit(toString(short), split = ""))
  
  shared = 0
  
  for (d in 1:length(short)){
    for (c in 1:length(long)) {
      if (short[d] == long[c]){
        shared = shared + 1
      }
    }
  }
  
  return(shared)
}

total = 0

while ( TRUE ) {
  tempt = unlist(strsplit(readLines(con, n = 1, warn = FALSE), split = " "))
  
  if (length(tempt) == 0) {
    break
  }
  
  clues = tempt[- ((length(tempt)-4):length(tempt))]
  fives = list()
  sixes = list()
  
  key = c(0:0, length = 10)
  
  code = tempt[- (1:(length(tempt)-4))]
  
  for (i in 1:length(clues)){
    current_length = length(unlist(strsplit(toString(tempt[i]), split = "")))
    
    if (current_length == 2){
      key[2] = tempt[i]
    } else if (current_length == 3){
      key[8] = tempt[i]
    } else if (current_length == 4){
      key[5] = tempt[i]
    } else if (current_length == 7){
      key[9] = tempt[i]
    } else if (current_length == 5){
      fives = append(fives, tempt[i])
    } else if (current_length == 6){
      sixes = append(sixes, tempt[i])
    }
  }
  
  #finding the code for 6
  for (i in 1:length(sixes)){
    if (number_shared(sixes[i], key[2]) == 1){
      key[7] = sixes[i]
      sixes = sixes[-i]
      break
    }
  }
  
  #finding the code for 0 (and therefore 9)
  for (i in 1:length(sixes)){
    if (number_shared(sixes[i], key[5]) == 3){
      key[1] = sixes[i]
      sixes = sixes[-i]
      break
    }
  }
  key[10] = sixes[1]
  
  #finding the code for 2
  for (i in 1:length(fives)){
    if (number_shared(fives[i], key[5]) == 2){
      key[3] = fives[i]
      fives = fives[-i]
      break
    }
  }
  
  #finding the code for 3
  for (i in 1:length(fives)){
    if (number_shared(fives[i], key[2]) == 2){
      key[4] = fives[i]
      fives = fives[-i]
      break
    }
  }
  
  key[6] = fives[1]
  
  #print(toString(key))
  
  add_to_total = 0
  
  for (c in 1:length(code)){
    #print(paste("Code:", c, toString(code[c])))
    current_length = length(unlist(strsplit(toString(code[c]), split = "")))
    
    for (k in 1:length(key)){
      key_length = length(unlist(strsplit(toString(key[k]), split = "")))
      
      if (key_length == current_length){
        if (number_shared(code[c], key[k]) == current_length){
          add_to_total = add_to_total + ((k-1) * (10 ^ (4-c)))
          break
        }
      }
    }
  }
  
  print(add_to_total)
  
  total = total + add_to_total
}

close(con)

print(total)

