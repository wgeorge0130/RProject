#Written by George Wang. 2021.

#1
#qqnorm(q1dataset$V1, main = "Normal Q-Q Plot: Concentration of An Antifungal Gel")
x = q1dataset$V1
xlog = log(x)
qqnorm(xlog, main = "Normal Q-Q Plot: Concentration of An Antifungal Gel")
qqline(xlog, col = "red")

# for (i in 1:5) {
#   vec <- numeric(1000) #preallocate a numeric vector
#   for (j in 1:5) { #fill the vector
#     vec[j] <- i^j 
#   }
#   mylist[[i]] <- vec #put all vectors in the list
# }
# df <- do.call("rbind",mylist) #combine all vectors into a matrix

#2
x = q2dataset$V1
qqnorm(x)
qqline(x, col = "red")
trans = 1/x
qqnorm(trans, main = "Normal Q-Q Plot: Reciprocal")
qqline(trans, col = "red")

#3
x = runif(1000, 0, 1)
y = runif(1000, 0, 1)
z = ((x-.5)^2+(y-.5)^2)
ifInside = NULL
for (i in 1:1000) { ifInside[i] = z[i] < 0.25 }
counter = 0
for (i in z) {if(i < 0.25) {counter = counter + 1} }
pie = 4*(counter/1000)
PE = ((abs((pie-3.14)))/3.14)*100
pieDiff = abs(3.14 - pie)
library("writexl")
write_xlsx(df, "C:\\Users\\wgeor_000\\Desktop\\Q3SimulationOutput")

#4
x1 = runif(1000, min = 0, max = 1)
y1 = runif(1000, min = 0, max = 1)
x2 = runif(1000, min = 0, max = 1)
y2 = runif(1000, min = 0, max = 1)
L = sqrt(((x2-x1)^2) + ((y2-y1)^2))
mean(L)
[1] 0.516851
mean = mean(L)
counter = 0
for (i in L) {if(i > 1) {counter = counter + 1} }
print(counter)
[1] 22
P = counter/1000
print(P)
[1] 0.022
PE = ((abs((.516851-.521405)))/.531405)*100
print(PE)
[1] 0.8569735
plot(x=x1, y=y1, main = "1000 Randomly Generated Endpoints of Path 1")
plot(x=x2, y=y2, main = "1000 Randomly Generated Endpoints of Path 1")
#df = data.frame(x1, x2, y1, y2, L, mean, counter, P, PE)

#5
lmda = 0.25616
x = list()
for(i in 1:1000) { x[[i]] = rexp(8, lmda)}
means = sapply(x, mean)
lambdas = 1/means
meanlambdas = mean(lambdas)
sdlambdas = sd(lambdas)
bias = meanlambdas - lmda
uncertainty = round(sdlambdas, digits = 2)
xvec = unlist(x)
hist(xvec, main = "1000 Randomly Generated Values via 'Rexp'")
print(length(xvec))

# means = list()
# for(j in 1:1000) { means[[j]] = mean(x[[j]])}
# lamdas = list()
# for(k in 1:1000) { lamdas[[k]] = 1/(means[[k]])}

#6
x = runif(1000, 8.3, 8.7)
y = runif(1000, 20.9, 21.5)
p = sqrt((x*y))
sd = sd(p, na.rm = FALSE)
mean = mean(p)
ndistribution = rnorm(1000, mean = mean, sd = sd)
qqnorm(ndistribution, main = "Normal Q-Q Plot: Intermediate Pressure")
qqline(ndistribution, col = "red")
hist(x, main = "1000 Randomly Generated Values: Pressure Entering", xlab = "X or Pressure Entering (in MPa)")
hist(y, main = "1000 Randomly Generated Values: Pressure Exiting", xlab = "Y or Pressure Exiting (in MPa)")
sortedNDist = sort(ndistribution)
CI1 = sortedNDist[25]
CI2 = sortedNDist[975]
CIlow = mean - 1.96*sd
CIup = mean + 1.96*sd