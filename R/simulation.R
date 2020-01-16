# random buy 3D
randomGen <- function(Date){
  n <- length(Date)
  num <- floor(runif(n,min = 0, max = 999.999))
  num3D <- formatC(num, width=3, flag="0")
  random3D <- data.frame('Date'=Date,'num3D'=num3D)
  return(random3D)
}

#############################################################################################
# random buy result
# randomBuy(Data)
randomBuy <- function(Data){
  dataRandom <- randomGen(Data$Date)
  data <- data.frame('Date'=Data$Date,'Data'=paste(Data$Num1,Data$Num2,Data$Num3,sep=''))
  dataAll <- data %>% left_join(dataRandom,by = 'Date')
  count <- 0
  for(i in 1:nrow(dataAll)){
    if(as.numeric(as.character(dataAll$Data[i])) == as.numeric(as.character(dataAll$num3D[i])))
      count <- count + 1
  }
  winRate <- count/nrow(dataAll)
  winMoney <- count*1040 - 2 * nrow(dataAll)
  result <- list('Date'=Data$Date, 'count'=count, 'winRate'=winRate, 'winMoney'=winMoney)
  class(result) <- 'winRate'
  return(result)
}

print.winRate <- function(winRate){
  n <- length(winRate$Date)
  cat('在过去的',n,'天里','\n')
  cat('您的中奖次数：',winRate$count,'\n')
  cat('您的中奖概率: ',winRate$winRate,'\n')
  cat('您的盈利率为',winRate$winMoney,'元')
}

#####################################################################################
# change string to character set
str2chrSet <- function(stringObj){
  strSet <- NULL
  for(i in 1:nchar(stringObj)){
    strSet <- c(strSet,substr(stringObj,i,i))
  }
  return(strSet)
}
# random buy combination 
randomBuyCom <- function(Data){
  dataRandom <- randomGen(Data$Date)
  data <- data.frame('Date'=Data$Date,'Data'=paste(Data$Num1,Data$Num2,Data$Num3,sep=''))
  dataAll <- data %>% left_join(dataRandom,by = 'Date')
  count6 <- 0
  count3 <- 0
  for(i in 1:nrow(dataAll)){
    data1 <- str2chrSet(as.character(dataAll$Data[i]))
    data2 <- str2chrSet(as.character(dataAll$num3D[i]))
    # 因为c(1,1,2)和c(1,2,2)在set上是相等的,所以要判断两个组合是否完全一样
    
    if(setequal(data1, data2) & length(table(data1)) == 3 & length(table(data2)) == 3)
      count6 <- count6 + 1
    if(setequal(data1, data2) & length(table(data1)) == 2 & length(table(data2)) == 2 ){
      if(sum(table(data1) == table(data2)) == 2){
        count3 <- count3 + 1
      }
    }
  }
  winRate <- (count3 + count6)/nrow(dataAll)
  winMoney <- count6*173 + count3*346 - 2 * nrow(dataAll)
  result <- list('Date'=Data$Date, 'count3'=count3, 'count6'=count6, 'winRate'=winRate, 'winMoney'=winMoney)
  class(result) <- 'winRateCom'
  return(result)
}
# randomBuyCom(Data = Data)

print.winRateCom <- function(winRate){
  n <- length(winRate$Date)
  cat('在过去的',n,'天里','\n')
  cat('您的中豹子号次数：',winRate$count1,'\n')
  cat('您的中组选3次数：',winRate$count3,'\n')
  cat('您的中组选6次数：',winRate$count6,'\n')
  cat('您的中奖概率: ',winRate$winRate,'\n')
  cat('您的盈利率为',winRate$winMoney,'元')
}


