display <- function(state){
  for (i in 1:length(state)){
    if (is.na(state[i])==TRUE){state[i]=i}
  }
  cat("",state[1],"|",state[2],"|",state[3],"\n")
  cat("---+---+---","\n")
  cat("",state[4],"|",state[5],"|",state[6],"\n")
  cat("---+---+---","\n")
  cat("",state[7],"|",state[8],"|",state[9],"\n")
}

update <- function(state, who, pos){
  if (is.na(state[pos])==FALSE) {print("This spot is already taken.")}
  else {state[pos] <- who}
  return(state)
}

computer_turn <- function(state){
  # determine computer plays x or o
  if(sum(is.na(state))%%2==0){computer <- "o"} else {computer <- "x"}
  test <- state
  canwin=FALSE
  canblock=FALSE
  if (computer =="o"){
    for(i in 1:9){
      if (is.na(test[i])==TRUE){
        test[i]="o"
        canwin <- check_winner(test)
        if(canwin==TRUE){
          state <- update(state,"o",i)
          cat("o moves",i,"\n")
          break}
        test <- state
      }
    }
    if(canwin==TRUE){return (state)}
    if(canwin==FALSE){
      for(i in 1:9){
        if (is.na(test[i])==TRUE){
          test[i]="x"
          canblock <- check_winner(test)
          if(canblock==TRUE){
            state <- update(state,"o",i)
            cat("o moves",i,"\n")
            break}
          test <- state
        }
      }
    }
    if(canblock==TRUE){return(state)}
    if(canblock==FALSE){
      for(i in 1:9){
        if(is.na(test[i])==TRUE){
          state[i]="o" 
          cat("o moves",i,"\n")
          break}
      }
    }
  }
  else if (computer =="x"){
    for(i in 1:9){
      if (is.na(test[i])==TRUE){
        test[i]="x"
        canwin <- check_winner(test)
        if(canwin==TRUE){
          state <- update(state,"x",i)
          cat("x moves",i,"\n")
          break}
        test <- state
      }
    }
    if(canwin==TRUE){return (state)}
    if(canwin==FALSE){
      for(i in 1:9){
        if (is.na(test[i])==TRUE){
          test[i]="o"
          canblock <- check_winner(test)
          if(canblock==TRUE){
            state <- update(state,"x",i)
            cat("x moves",i,"\n")
            break}
          test <- state
        }
      }
    }
    if(canblock==TRUE){return(state)}
    if(canblock==FALSE){
      for(i in 1:9){
        if(is.na(state[i])==TRUE){
          state[i]="x" 
          cat("x moves",i,"\n")
          break}
      }
    }
  }
  return (state)
}

check_winner <- function(state){
  for (i in 1:length(state)){
    if (is.na(state[i])==TRUE){state[i]=i}
  }
  if((state[1]=="x" && state[2] =="x" && state[3]=="x")
     ||(state[4]=="x" && state[5] =="x" && state[6]=="x")
     ||(state[7]=="x" && state[8] =="x" && state[9]=="x")
     ||(state[1]=="x" && state[4] =="x" && state[7]=="x")
     ||(state[2]=="x" && state[5] =="x" && state[8]=="x")
     ||(state[3]=="x" && state[6] =="x" && state[9]=="x")
     ||(state[1]=="x" && state[5] =="x" && state[9]=="x")
     ||(state[3]=="x" && state[5] =="x" && state[7]=="x")){
    return(TRUE)
  }
  else if ((state[1]=="o" && state[2] =="o" && state[3]=="o")
           ||(state[4]=="o" && state[5] =="o" && state[6]=="o")
           ||(state[7]=="o" && state[8] =="o" && state[9]=="o")
           ||(state[1]=="o" && state[4] =="o" && state[7]=="o")
           ||(state[2]=="o" && state[5] =="o" && state[8]=="o")
           ||(state[3]=="o" && state[6] =="o" && state[9]=="o")
           ||(state[1]=="o" && state[5] =="o" && state[9]=="o")
           ||(state[3]=="o" && state[5] =="o" && state[7]=="o")){
    return(TRUE)
  }
  else return(FALSE)
}

check_draw <- function(state){
  if (sum(is.na(state))==0 && check_winner(state)==FALSE){return(TRUE)}
  else{return(FALSE)}
}

play <- function(){
  num_player <- readline("How many human players? 1 or 2: ")
  state <- rep(NA,9)
  if (as.integer(num_player) == 2) {
    win_state = FALSE
    draw_state=FALSE
    display(state)
    while(!win_state){
      x_move <- readline("Where should x play: ")
      state <- update(state, "x",as.integer(x_move))
      win_state = check_winner(state)
      draw_state = check_draw(state)
      if(win_state){cat("x wins","\n")}
      display(state)
      if(draw_state){
        cat("Draw","\n")
        break}
      if (!win_state){
        o_move <- readline("Where should o play: ")
        state <- update(state,"o",as.integer(o_move))
        win_state = check_winner(state)
        draw_state=check_draw(state)
        if(win_state){cat("o wins","\n")}
        display(state)
        if(draw_state){
          cat("Draw","\n")
          break}
      }
    }
  }
  else if (as.integer(num_player)==1){
    computer_round <- readline("Should the computer play first or second? 1 or 2: ")
    win_state = FALSE
    draw_state=FALSE
    display(state)
    if (as.integer(computer_round)==1){
      while(!win_state){
        state <- computer_turn(state)
        win_state = check_winner(state)
        draw_state=check_draw(state)
        if(win_state){cat("x wins","\n")}
        display(state)
        if(draw_state){
          cat("Draw","\n")
          break}
        if(!win_state){
          user_move <- readline("Where should o play: ")
          state <- update(state,"o",as.integer(user_move))
          win_state=check_winner(state)
          draw_state=check_draw(state)
          if(win_state){cat("o wins","\n")}
          display(state)
          if(draw_state){
            cat("Draw","\n")
            break}
        }
      }
    }else if (as.integer(computer_round)==2){
      while(!win_state){
        user_move <- readline("Where should x play: ")
        state <- update(state,"x",as.integer(user_move))
        win_state=check_winner(state)
        draw_state=check_draw(state)
        if(win_state){cat("x wins","\n")}
        display(state)
        if(draw_state){
          cat("Draw","\n")
          break}
        if(!win_state){
          state <- computer_turn(state)
          win_state=check_winner(state)
          draw_state=check_draw(state)
          if(win_state){cat("o wins","\n")}
          display(state)
          if(draw_state){
            cat("Draw","\n")
            break}
        }
      }
    }
  }
}

#####check code######
teststate <- rep(NA,9)
display(teststate)
computer_turn(teststate)

teststate <- c("x", NA, NA, NA, "o", "x", NA, "o", "x")
display(teststate)
computer_turn(teststate)

teststate <- c("x", "x", NA, "o", "o", NA, NA, "o", "x")
display(teststate)
computer_turn(teststate)

teststate <- c("x", NA, NA, NA, "o", "x", NA, "o", "x")
newstate <- update(teststate, "o", 2)
display(newstate)

teststate <- c("x", NA, NA, NA, "o", "x", NA, "o", "x")
newstate <- update(teststate, "o", 1)

teststate <- c("x", "x", NA, "o", "o", NA, NA, "o", "x")
check_winner(teststate)

teststate <- c("x", "x", "x", "o", "o", NA, NA, "o", "x")
check_winner(teststate)

teststate <- c("x", "x", "o", "o", "o", "x", "x", "o", "x")
check_winner(teststate)