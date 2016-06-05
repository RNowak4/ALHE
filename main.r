##########################################################################
# Mając aktualne dane dotyczące cen biletów lotniczych i miejsc hotelowych, 
# oraz historyczne informacje pogodowe, zaproponować metodę optymalizującą
# wyjazd urlopowy o podanej długości.
#
# Wojciech Przechodzen
# Radoslaw Nowak
##########################################################################

# Generowanie danych dotyczacych dni urlopowych
# startDay - nr dnia, od ktorego zaczynamy planowac urlop
getActualData <- function() {
  day.data <- matrix(nrow = 365, ncol = 3)
  for(i in 1:365) {
    day.data[i, 1] <- rnorm(n = 1, mean = 400, sd = 40)
    day.data[i, 2] <- rnorm(n = 1, mean = 30, sd = 5)
    day.data[i, 3] <- i
  }
  
  return(day.data)
}

# Temperatura w ciagu ostatnich 10 lat
history.temperature <- matrix(data = runif(10, min = 5, max = 40), ncol = 365, nrow = 10)
#history.temperature <- B = matrix(c(40, 40, 40, 40, 40, 40), nrow=1, ncol=60) 
# Pogoda w ciagu ostatnich 10 lat
history.weather <- matrix(data = floor(runif(10, min = 0, max = 10)), ncol = 365, nrow = 10)

utils.initHistory<-function(n) {
  history<-data.frame(startDay = numeric(n), score = numeric(n))
  
  return (history)
}

utils.historyPush<-function(i, newDay, history) {
  if (i <= nrow(history)) {
    history[i,] = newDay
  }
  
  return (history)
}

# Zalezy jak zdefiniujemy sasiedztwo...
utils.getNeighbours <- function(day, len, startDay, endDay) {
  neighbours <- c()
  
  diff <- day - startDay
  if(diff > 2) 
    diff <- 2
  else if(diff < 0)
    diff <- 0
  for(i in 1:diff) {
    neighbours <- c(neighbours, day - i)
  }
  
  diff <- endDay - (day + len)
  if(diff > 2) 
    diff <- 2
  else if(diff < 0)
    diff <- 0
  for(i in 1:diff) {
    neighbours <- c(neighbours, day + i)
  }
  
  return(neighbours)
}

utils.getAverageTemperature <- function(day) {
  result <- 0.0
  
  for(i in 1:nrow(history.temperature)) {
    result <- result + history.temperature[i, day]
  }
  
  return(result / (nrow(history.temperature)*1.0))
}

utils.getAverageWeather <- function(day) {
  result <- 0.0
  
  for(i in 1:nrow(history.weather)) {
    result <- result + history.weather[i, day]
  }
  
  return(result / (nrow(history.weather)*1.0))
}

utils.scoreDay <- function(actualData, day, len) {
  endDay <- day + len
  planeTicketPrice <- actualData[day, 1] + actualData[endDay, 1]
  hotelPrice <- 0.0
  weatherScore <- 0.0
  temperatureScore <- 0.0
  
  for(i in day:endDay) {
    hotelPrice <- hotelPrice + actualData[i, 2]
    actualDay <- actualData[i, 3]
    weatherScore <- utils.getAverageTemperature(i) * utils.getAverageWeather(i)
  }
  
  score <- 1000000 - (planeTicketPrice + hotelPrice) + weatherScore
  score <- 1
  
  return(score)
}

utils.termination <- function(i) {
  if(i > 70)
    return(TRUE)
  
  return(FALSE)
}

utils.getRandomNeighbour <- function(day, len, startDay, endDay) {
  neighbours <- utils.getNeighbours(day = day, len = len, startDay =  startDay, endDay = endDay)
  
  return(neighbours[floor(runif(n = 1, min = 1, max = length(neighbours)))])
}

alghoritm.findOptimalHolidays <- function(startDay, endDay, len, startTemp) {
  actualData = getActualData()
  
  # Algorytm symulowanego wyzarzania
  s0 <- floor(runif(n = 1, min = startDay, max = endDay))
  sx <- utils.scoreDay(day = s0, actualData = actualData, len = len)
  history <- utils.initHistory(n = 20)
  x <- s0
  best <- x
  T <- startTemp
  i <- 0
  while(!utils.termination(i)) {
    y <- utils.getRandomNeighbour(day = x, len = len, startDay = startDay, endDay = endDay)
    sy <- utils.scoreDay(day = y, actualData = actualData, len = len)
    if(sy > sx) {
      x <- y
      sx <- sy
      best <- x
    } else {
      pa <- exp(-abs(sy - sx) / T)
      r <- runif(n = 1, min = 0, max = 1)
      if(r < pa) {
        x <- y
        sx <- sy
      }
    }
    i <- i + 1
    T <- T - T/2
  }
  
  return(best)
}

alghoritm.findOptimalHolidays(1, 100, 30, 0.01)
alghoritm.findOptimalHolidays(1, 100, 30, 0.01)
alghoritm.findOptimalHolidays(1, 100, 30, 0.01)
alghoritm.findOptimalHolidays(1, 100, 30, 0.01)
alghoritm.findOptimalHolidays(1, 100, 30, 0.01)

