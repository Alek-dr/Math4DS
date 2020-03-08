library(ggplot2)

# Ефимов. Т.4

# Пример стр. 186
x <- c(5,3,7,10,5,5,2,10,7,2,7,7,4,2,4)
n <- length(x)

# Вариационый ряд
V <- sort(x)
# w - размах
w <- max(x) - min(x)
# Статистический ряд
t <- table(x)

ggplot(mapping =  aes(x)) + geom_histogram(bins = 20)

# Пример стр. 188
M <- c(20.3, 15.4, 17.2, 19.2, 23.3, 18.1, 21.9,
       15.3, 16.8, 13.2, 20.4, 16.5, 19.7, 20.5,
       14.3, 20.1, 16.8, 14.7, 20.8, 19.5, 15.3,
       19.3, 17.8, 16.2, 15.7, 22.8, 21.9, 12.5,
       10.1, 21.1, 18.3, 14.7, 14.5, 18.1, 18.4,
       13.9, 19.1, 18.5, 20.2, 23.8, 16.7, 20.4,
       19.5, 17.2, 19.6, 17.8, 21.3, 17.5, 19.4,
       17.8, 13.5, 17.8, 11.8, 18.6, 19.1)

freq_grouped_values <- function(x,n_intervals){
  len <- length(x)
  max_ <- max(x)
  min_ <- min(x)
  w <- max_ - min_
  b <- round(w/n_intervals)
  breaks <- floor(seq(min_,max_+b,b))
  mid <- seq(min(breaks)+b/2, max(breaks), b)
  Inerval <- cut(x, breaks = breaks)
  t <- data.frame(table(Inerval))
  t$Midpoint <- mid
  t$Rel_freq <- round(t$Freq / len,4)
  t$Cum_sum <- cumsum(t$Freq)
  t$Rel_c_sum <-round(t$Cum_sum / len, 4)
  t <- t[, c(1,3,2,5,4,6)]
  return(t)
}

n_intervals <- 7
w <-  max(M)-min(M)
b <-  round(w/n_intervals)
start <- round(min(M))
end <-round(max(M))
T <- freq_grouped_values(M, n_intervals)

ggplot(data=D, aes(x=D$M)) + 
  geom_histogram(aes(y=stat(count/b)), breaks=seq(start,end,by=b), col='black') + 
  scale_x_continuous(breaks = seq(start,end,by=b)) + 
  labs(title="Гистограмма частот", x="x", y="n*/2")

ggplot(data=D, aes(x=D$M)) + 
  geom_freqpoly(aes(y=stat(count/b)), breaks=seq(start,end,by=b), col='black', linetype = "solid") +
  scale_x_continuous(breaks = seq(start,end,by=b)) + 
  labs(title="Полигон частот", x="x", y="n*")

ggplot(data=T, aes(x=T$Midpoint,y=T$Freq/b)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks = seq(start,end,by=b))+
  labs(title="Полигон частот", x="x", y="n*")

ggplot(data=D, aes(D$M)) + stat_ecdf(geom = "step") + labs(title="Эмперическая функция распределения", x="x", y="F*")

Fn <- ecdf(M)
breaks=seq(start,end,by=b)
p <- Fn(breaks)
Fn <- data.frame(breaks,p)

ggplot(data=Fn, aes(x=Fn$breaks, y=Fn$p)) +
  geom_bar(stat="identity", width = 2, col='black', position = position_nudge(x = 0)) + 
  scale_x_continuous(breaks = seq(start,end,by=b)) + 
  labs(title="Эмперическая функция распределения", x="x", y="F*")


