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
getActualData <- function(startDay, endDay) {
  n <- endDay - startDay + 1
  
  day.data <- data.frame(
    ticket_price = c(rnorm(n, mean = 400, sd = 40)),
    hotel_price = c(rnorm(n, mean = 30, sd = 5)),
    dayNr = seq(startDay, endDay, by = 1)
  )
  
  return(day.data)
}

# Temperatura w ciagu ostatnich 10 lat
history.temperature <- matrix(data = runif(10, min = 5, max = 40), ncol = 365, nrow = 10)

# Pogoda w ciagu ostatnich 10 lat
history.weather <- matrix(data = floor(runif(10, min = 0, max = 10)), ncol = 365, nrow = 10)

utils.initHistory<-function(n, score) {
  history<-data.frame(startDay = numeric(n), score = numeric(score))
  
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
  
  diff <- abs(day - startDay)
  for(i in 1:(diff > 2?2:diff)) {
    neighbours <- c(neighbours, day - i)
  }
  
  diff <- abs(day + len - endDay)
  for(i in 1:(diff > 2?2:diff)) {
    neighbours <- c(neighbours, day + i)
  }
  
  return(neighbours)
}

utils.getAverageTemperature(day) {
  result <- 0.0
  
  for(i in 1:10) {
    result <- result + history.temperature[day, i]
  }
  
  return(result / 10.0)
}

utils.getAverageWeather(day) {
  result <- 0.0
  
  for(i in 1:10) {
    result <- result + history.weather[day, i]
  }
  
  return(result / 10.0)
}

utils.scoreDay <- function(actualData, day, len) {
  endDay <- day + len
  planeTicketPrice <- actualData[1, 1] + actualData[len, 1]
  hotelPrice <- 0.0
  weatherScore <- 0.0
  temperatureScore <- 0.0
  
  for(i in day:endDay) {
    hotelPrice <- hotelPrice + actualData[i, 2]
    actualDay <- actualData[i, 3]
    # TODO ...
    #weatherScore <- 
  }
  
  score <- exp(-(planeTicketPrice + hotelPrice))
  
  return(score)
}

utils.termination <- function(i) {
  if(i > 20)
    return(TRUE)
  
  return(FALSE)
}

utils.getRandomNeighbour <- function(day, len, startDay, endDay) {
  neighbours <- utils.getNeighbours(day = day, len = len, startDay =  startDay, endDay = endDay)
  
  return(neighbours[floor(runif(min = 1, max = lenght(neighbours)))])
}

alghoritm.findOptimalHolidays <- function(startDay, endDay, len, startTemp) {
  actualData = getActualData(startDay = startDay, endDay = endDay)
  
  # Algorytm symulowanego wyzarzania
  s0 <- startDay + (runif(min = 0, max = len))
  sx <- utils.scoreDay(day = s0, actualData = actualData, len = len)
  history <- utils.initHistory(n = s0, score = sx)
  x <- s0
  T <- startTemp
  while(!utils.termination(i)) {
    y <- utils.getRandomNeighbour(day = x, len = len, startDay = startTemp, endDay = endDay)
    sy <- utils.scoreDay(day = y, actualData = actualData, len = len)
    if(sy > sx) {
      x <- y
      sx <- sy
    } else {
      pa <- exp(-abs(sy - sx) / T)
      r <- runif(min = 0, max = 1)
      if(r < pa) {
        x <- y
        sx <- sy
      }
    }
    # T <- T - ...
  }
  
  return(x)
}