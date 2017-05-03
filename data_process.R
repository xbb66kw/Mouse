rm(list=ls()); # clear all variables
graphics.off() #

library(R.matlab)
library(quantreg)
#data_ <- readMat("/Users/xbb/Desktop/mouse/mat_files/an171923_2012_06_04_data_struct.mat")
data_ <- readMat("/Users/xbb/Desktop/mouse/mat_files/an198503_2013_03_01_data_struct.mat")
#an198503_2013_03_01_data_struct.mat : 1301 nuerons, 400 trials.
mouse <- data_

b <- 3; # b <- 1 is for wisker moving
A <- mouse$s[[10]][[3]][[b]][[1]];
#A <- mouse$s[[10]][[3]][[b]][[1]];
A_ <- c(0,90);
B_ <- c(-0.4, 2);
#A[[6]]
col_index <- c('blue', 'black', 'red', 'green3', 'gray3', 'purple', 'yellow3',
'antiquewhite',
'antiquewhite1',
'antiquewhite2',
'antiquewhite3',
'antiquewhite4',)

col_index <- c('blue', 'black')

mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
demean <- function(x){x - mean(x)}

M <- 0
for  (a in unique(A[[6]][1,])) {
				if (!is.nan(mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == a ][1])) {
								plot(1:sum(A[[6]] == a), M +  (mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == a ]), type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = col_index[floor(a %% length(col_index)) + 1]) # remember to scale
								
								M <- M + 1.5

								par(new = TRUE);

				}
}
par(new = FALSE);


#pdf("/Users/xbb/Desktop/mouse_data_plot3.pdf ")



#reward cues
reward_timing <- mouse[[1]][11][[1]][3][[1]][[7]][[1]][6][[1]][1,]
reward_trials <- mouse[[1]][11][[1]][3][[1]][[7]][[1]][7][[1]][1,]

#touches times
touches_timing <- mouse[[1]][11][[1]][3][[1]][[2]][[1]][5][[1]][[1]][[1]][1,]#Touches times
touches_trials <- mouse[[1]][11][[1]][3][[1]][[2]][[1]][6][[1]][[1]][[1]][1,]#Touches Trials

#Pole rising times
pole_timing <- mouse[[1]][11][[1]][3][[1]][[1]][[1]][6][[1]][1,]
pole_trials <- mouse[[1]][11][[1]][3][[1]][[1]][[1]][7][[1]][1,]


#Setup the nueron data right after the reward cues:
region <- 2
mid_ <- mouse[[1]][10][[1]][3][[1]][[region]][[1]]


#A_ <- c(0,40);
B_ <- c(10, 15);

unique(mouse[[1]][10][[1]][3][[1]][[region]][[1]][6][[1]][1,]) # j's range.
j = 357
#373# Say.




x_axis <- mid_[5][[1]][1,][mid_[6][[1]][1,] == j]#x-asix coordinates. times
M <- 0
#mid_[[1]][1,] are the ids
for (k in 1:length(mid_[[1]][1,])) {
								
				response <- mid_[[7]][k,][which(mid_[6][[1]][1,] == j)]
				
				plot(x_axis, M +  response, type = 'l', ylim = B_, cex = 0.1, col = col_index[floor(k %% length(col_index)) + 1]) # remember to scale

				
				M <- M + 1.5
				par(new = TRUE);
}
for (k in touches_timing[touches_trials == j]) { abline(v = k)}
for (k in reward_timing[reward_trials == j]) { abline(v = k, col = 'gray')}
for (k in pole_timing[pole_trials == j]) { abline(v = k, col = 'purple')}
abline(v = pole_timing[pole_trials == j] + 6000, col = 'green')
par(new = FALSE)











#the same type test:

plot(1:length(y), mav(y), type = 'l', col = 'blue')
par(new = TRUE)
plot(1:length(y), y, type = 'l', col = 'black')

#Data extracted and experiments, with moving average:
#a <- 6, 7;
par(new = FALSE)
a <- unique(A[[6]][1,])[17]
index_ <- unique(A[[6]][1,])[26]
a6 <- mav(mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == a ])
a7 <- mav(mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == index_ ])
len_ <- min(length(a6), length(a7));
a6 <- a6[1:len_]
a7 <- a7[1:len_]
test_ <- !(is.na(a6) | is.na(a7));
a6 <- a6[test_]
a7 <- a7[test_]

plot(lm(a6~a7)$fitte, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'black')
par(new = TRUE)
plot(a6, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'red')
par(new = TRUE)
plot(a7, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'purple4')
par(new = TRUE)
plot(rq(a6~a7)$fitte, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'green4')
par(new = TRUE)
#plot(a6, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'red')

#plot(a6, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'red')
sum(lm(a6~a7)$resi^{2})  + sum(lm(a6~a7)$resi^{2}) / len_ * log(len_) * 1
sum(lm(a6~1)$resi^{2}) #on;y intercept
#sum(lm(a6~1)$resi^{2}) / len_ * log(len_) * 0

sum(abs(rq(a6~a7)$resi))  + sum(abs(rq(a6~a7)$resi)) / len_ * sqrt(log(len_)) * 1
sum(abs(rq(a6~1)$resi)) #only intercept
#sum(abs(rq(a6~1)$resi)) / len_ * sqrt(log(len_)) * 0






######
######
######
######Test whether they are in the same group:
#Data extracted and experiments, withOUT moving average:
#a <- 6, 7;12,13; 12, 16are good examples
par(new = FALSE)
a <- unique(A[[6]][1,])[12]
index_ <- unique(A[[6]][1,])[13]
a6 <- (mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == a ])
a7 <- (mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == index_ ])
len_ <- min(length(a6), length(a7));
a6 <- a6[1:len_]
a7 <- a7[1:len_]
test_ <- !(is.na(a6) | is.na(a7));
a6 <- a6[test_]
a7 <- a7[test_]
len_ <- min(length(a6), length(a7));
plot(lm(a6~a7)$fitte, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'black')
par(new = TRUE)
plot(a6, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'red')
par(new = TRUE)
plot(a7, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'purple4')
par(new = TRUE)
plot(rq(a6~a7)$fitte, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'green4')

#plot(a6, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'red')
sum(lm(a6~a7)$resi^{2})  + sum(lm(a6~a7)$resi^{2}) / len_ * log(len_) * 1
sum(lm(a6~1)$resi^{2}) #on;y intercept
#sum(lm(a6~1)$resi^{2}) / len_ * log(len_) * 0

sum(abs(rq(a6~a7)$resi))  + sum(abs(rq(a6~a7)$resi)) / len_ * sqrt(log(len_)) * 1
sum(abs(rq(a6~1)$resi)) #only intercept
#sum(abs(rq(a6~1)$resi)) / len_ * sqrt(log(len_)) * 0

##### Ar test


#Evidence that moving average might not work. Maybe we can directly remove those series that can be self-explained.
f_ <- 0.1
a6 <- mav(f_ * arima.sim(80, model = list(ar = c(0.3))))
a7 <- mav(f_ * arima.sim(80, model = list(ar = c(0.3))))
len_ <- min(length(a6), length(a7));
a6 <- a6[1:len_]
a7 <- a7[1:len_]
test_ <- !(is.na(a6) | is.na(a7));
a6 <- a6[test_]
a7 <- a7[test_]
len_ <- min(length(a6), length(a7));
plot(lm(a6~a7)$fitte, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'black')
par(new = TRUE)
plot(a6, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'red')
par(new = TRUE)
plot(a7, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'purple4')
par(new = TRUE)
plot(rq(a6~a7)$fitte, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'green4')

#plot(a6, type = 'l', xlim = A_, ylim = B_, cex = 0.1, col = 'red')
sum(lm(a6~a7)$resi^{2})  + sum(lm(a6~a7)$resi^{2}) / len_ * log(len_) * 1
sum(lm(a6~1)$resi^{2}) #on;y intercept
#sum(lm(a6~1)$resi^{2}) / len_ * log(len_) * 0

sum(abs(rq(a6~a7)$resi))  + sum(abs(rq(a6~a7)$resi)) / len_ * sqrt(log(len_)) * 1
sum(abs(rq(a6~1)$resi)) #only intercept
#sum(abs(rq(a6~1)$resi)) / len_ * sqrt(log(len_)) * 0


##Ar and a7 to explain a6
#par(new = FALSE)
order_ <- 2
y <- a6[(order_ + 1):length(a6)]

results <- rep(1, length(a6) - order_)
for (k in 1:order_) {
				results <- cbind(results, a7[k:(length(a7) - order_ - 1 + k)])
}
results <- a7[1:(length(a7) - 2)]

X <- cbind(results, a6[order_:(length(a6)-1)])
sum((y - X %*% solve(t(X) %*% X) %*% t(X) %*% y)^{2}) / length(a6)
sum((y - X %*% solve(t(X) %*% X) %*% t(X) %*% y)^{2})
X <- cbind(1, a6[order_:(length(a6)-1)])
sum((y - X %*% solve(t(X) %*% X) %*% t(X) %*% y)^{2}) / length(a6)

X <- cbind(results, a6[order_:(length(a6)-1)])
#X <- cbind(1, a6[order_:(length(a6)-1)])
#par(new = TRUE)
plot(X %*% solve(t(X) %*% X) %*% t(X) %*% y, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'gray4')

for (i in 1:20) {
				Y <- arima.sim(80, model = list('ar' = c(0.25, 0.21, 0.24))) * 0.15 #- 0.0993
				#par(new = TRUE)
				plot(Y, type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'gray4')
				Sys.sleep(0.2)
}
par(new = TRUE)
plot(mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == 3 ], type = 'l', xlim = A_, ylim = B_, cex = 0.3, col = 'blue4')
arima(mouse$s[[10]][[3]][[b]][[1]][[7]][1,][A[[6]] == 3 ], order = c(3,0,0))
arima(Y, order = c(3,0,0))