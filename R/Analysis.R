# random buy 3D
randomGen <- function(Date){
  n <- length(Date)
  num <- floor(runif(n,min = 0, max = 999.999))
  num3D <- formatC(num, width=3, flag="0")
  random3D <- data.frame('Date'=Date,'num3D'=num3D)
  return(random3D)
}

# Compare the random 
CompareData <- function(Data){
  dataRandom <- randomGen(Data$Date)
  data <- data.frame('Date'=Data$Date,'Data'=paste(Data$Num1,Data$Num2,Data$Num3,sep=''))
  dataAll <- data %>% left_join(dataRandom,by = 'Date')
  count <- 0
  for(i in 1:nrow(dataAll)){
    if(as.numeric(dataAll$Data[i]) == as.numeric(dataAll$num3D[i]))
      count <- count + 1
  }
  winRate <- count/nrow(dataAll)
  winMoney <- count*1000 - 2 * nrow(dataAll)
  result <- list('Date'=Data$Date, 'count'=count, 'winRate'=winRate, 'winMoney'=winMoney)
  class(result) <- 'winRate'
  return(result)
}

print.winRate <- function(winRate){
  n <- length(winRate$Date)
  cat('在过去的',n,'天里','\n')
  cat('您的中将次数：',winRate$count,'\n')
  cat('您的中奖概率: ',winRate$winRate,'\n')
  cat('您的盈利率为',winRate$winMoney,'元')
}