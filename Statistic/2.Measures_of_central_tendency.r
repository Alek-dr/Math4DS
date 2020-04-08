library(psych)
library(seewave)
library(intervals)
# Пример 1. Записать в виде вариационного и статистического рядов 
# Определить размах выборки.
# Сборник задач А.В.Ефимова и А.С.Попелова, гл.19
x <- c(5,3,7,10,5,5,2,10,7,2,7,7,4,2,4)
n <- length(x)
# Вариационый ряд
V <- sort(x)
# w - размах
w <- max(x) - min(x)
# Статистический ряд
t <- table(x)
------------------------------------------------------------------------------------------
# Определить среднее, моду медиану выборки
# Сборник задач А.В.Ефимова и А.С.Попелова, гл.19
M <- c(5,6,8,2,3,1,1,4)

my_mode <- function(x){
  # table вернет таблицу частот
  t <- table(x)
  t <- sort(t,decreasing = TRUE)
  val <- names(t[1])
  return(as.numeric(val))
}

my_rms <- function(x){
  return(sqrt(sum(x^2)/length(x)))
}

mx <- mean(M)
hx <- median(M)
dx <- my_mode(M)
#------------------------------------------------------------------------------------------
# Примеры из Schaum's Outline of Theory and Problems of Statistics 3rd Edition by Murray R Spiegel, Larry J Stephens
x <- c(2,4,8)
# Среднее геометрическое
g <- geometric.mean(x)
# Среднее гармоническое
h <- harmonic.mean(x)
# Среднее квадратическое (root mean square):
c <- c(1,3,4,5,7)
# можно использовать их пакета seewave
rms1 <- rms(c)
# но можно и самому написать
rms2 <- my_rms(c)
rms1==rms2
