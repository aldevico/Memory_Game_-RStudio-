par(mfrow = c(1,1))
Random<-function(){
  Row <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
  Col <- c(1,2,3,4)
  Play <-array(dim= c(13,4))
  Num = nrow(Play)*ncol(Play)
  if ( Num %% 2 == 1) {stop("number of Row * number of Column must be even number")}
  df1<- expand.grid(Row, Col)
  df2<<- df1[sample(1:nrow(df1),52), ]
}

gridPlot<-function(){
  Logo <- c("1","2","3","4","5","6","7","8","9","10","11","12","13")
  Col <- c("1","2","3","4","5","6","7","8")
  n <- length(Logo)*length(Col)*2
  if (n > 208) {stop("Too many cards: number of Row * number of Column must be even number")}
  Loco <- expand.grid(Logo, Col)
  Loco1 <- Loco[sample(1:nrow(df1),26), ]
  Loco2 <- Loco1
  
  BindLoco <- rbind(Loco1,Loco2)
  colnames(BindLoco) <- c("Logo", "Color")
  gridData <<- cbind(df2,BindLoco)
  
  plot (df2, xlim=c(0,14),ylim=c(0,5),pch=22,cex= 7, xlab="X",ylab="Y", axes=F)
  box()
  par(bg = "white")
  text(x=c(1:13), rep(0,13),labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13"),cex=0.8)
  text(y=c(1:4), rep(0,4),labels = c("1","2","3","4"),cex=0.8)
  text(7,5,labels = "MEMORY GAME")
}

gameIntro<-function() {
  cat("Player 1 starts!\n In each move you have to choose two cards")
  player <<- "Player 1"
  foe <<- "Player 2"
  player1<<- "Player 1"
  player2<<- "Player 2"
}

gameChoice <- function(player){
  
  cat(sprintf("\n %s, choose your first card (1: row, 2: column)!", player))
  x1<<-readline("1: ")
  y1<<-readline("2: ")
  
  p1<-gridData[which(x1 == gridData$Var1), ]
  p2<-gridData[which(y1 == gridData$Var2), ] 
  p3<<-merge(p1,p2)
  points(x1,y1, pch=as.numeric(as.character(p3$Logo)) , cex=3, col=as.numeric(as.character(p3$Color)) )
  
  cat(sprintf("\n %s, choose your second card (1: row, 2: column)!", player))
  x2<<-readline("1: ")
  y2<<-readline("2: ")
  
  q1<-gridData[which(x2 == gridData$Var1), ]
  q2<-gridData[which(y2 == gridData$Var2), ] 
  q3<<-merge(q1,q2)
  points(x2,y2, pch=as.numeric(as.character(q3$Logo)) ,
         cex=3,col=as.numeric(as.character(q3$Color)) )
  
}

Symbol <-function(player,foe) {
  point<<-0
  false<<-FALSE
  matched <<-FALSE
  if(p3$Logo==q3$Logo && p3$Color==q3$Color){
    cat(sprintf("Correct! %s Plays again", player))
    y<<-readline("Press [y] to continue")
    
    points(x1,y1, pch=as.numeric(as.character(p3$Logo)) , cex=3, col=as.numeric(as.character(p3$Color)))
    points(x2,y2, pch=as.numeric(as.character(q3$Logo)) , cex=3, col=as.numeric(as.character(q3$Color)))
    point <<-point+1 # this is everytime there's a match it will add up 
    matched <<-TRUE
    done <<- rbind(p3,q3)
  }else{
    
    cat(sprintf("%s is wrong !", player))
    cat(sprintf("%s plays!", foe))
    y<-readline("Press [y], when you are ready to continue ")
    false<<-TRUE
  }
  if(point == 26){
    text(7,4.5,labels = "You Win!")
  }
}

# points(x1,y1, pch=15, cex=5.5, col= "white")
#points(x2,y2, pch=15 , cex=5.5, col= "white")
#points(7, 4.5, pch = 15, cex=5, col= "white")

CurrentLeaderboard<-function(point1,point2,player1 ,player2 ) {
  board <- data.frame(v1= point1,v2= point2)
  colnames(board) <- c(player1,player2)
  if (point1 > point2)
  {cat(sprintf("Correct! %s Plays again, Current Leaderboard:\n",player1))}
  else
  {cat(sprintf("Correct! %s Plays again, Current leaderboard:\n",player2))}
  
  print(board)
}

memory<-function(){
  p<<-0
  point1<-0
  point2<-0
  Random()
  gridPlot()
  gameIntro()
  player <<- player1
  foe <<-player2 
  
  while(point != 26){
    
    if(false == TRUE) {
      if (player == player1) {
        player <<- player2
        foe <<- player1
        false <<- FALSE
      }else if (player == player2) {
        player <<- player1
        foe <<- player2
        false <<- FALSE
      }
    }else if (matched == TRUE) {
      if (player == player1) {
        point1<-point1+1
        matched <<- FALSE
        CurrentLeaderboard(point1,point2, player1, player2)
      }else if (player == player2) {
        point2<-point2+1
        matched <<- FALSE
        CurrentLeaderboard(point1,point2, player1, player2)
      }
    }
    gameChoice(player)
    Symbol(player,foe)
  }
}

FinalLeaderboard<-function(point1,point2,player1 ,player2 ) {
  board <- data.frame(v1= point1,v2= point2)
  colnames(board) <- c(player1,player2) 
  if (point == 26) {  
    if (point1>point2){
      cat(sprintf("%s wins, Final Leaderboard:\n",player1))
      }
  else if (point2>point1){
    cat(sprintf("%s wins, Final leaderboard:\n",player2))
    }
  else if(point2==point2){
    cat(sprintf("%s wins, Final leaderboard:\n",player1,player2))
    }
    
    }

  
  print(board)
}

###
memory()
