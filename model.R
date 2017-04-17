lekOrg = c(2.26,1.81,1.78,1.54,2.06,1.74,2.56,2.29,1.80,2.32,2.04,1.88,1.18,2.08,1.70,1.74,1.90,1.79,2.11,1.72,1.74,1.60,2.15,2.26,1.65,1.63,2.40,2.70,1.90,2.78,2.27,1.74,2.62,1.80,1.81,1.58,2.41,1.65,2.24,1.70,2.45,1.72,2.37,2.23,1.92,1.99,1.99,2.35,1.80,2.36,1.59,2.10,1.80) 
sbpOrg = c(66,52,72,67,69,71,88,68,59,73,68,58,61,68,69,55,67,67,68,59,68,63,65,72,58,69,70,73,56,83,67,84,68,64,60,62,76,60,60,59,84,66,68,65,69,72,63,56,70,69,60,51,61)
czasOrg = c(7,10,18,4,10,13,21,12,9,65,20,31,23,22,13,9,50,12,11,8,26,16,23,7,11,8,14,39,28,12,60,10,60,22,21,14,4,27,26,28,15,8,46,24,12,25,45,72,25,28,10,25,44)

file= read.table("data.csv")
#data <- file[-c(10,17,31,48,33,53,13,47,43,1,37,24,2,30,8,27,16),]
#data <- file[-c(10,17,13,32,39,52),]
data <-file
lek = data[,1]
sbp = data[,2]
czas = data[,3]


X = cbind(1,lek,sbp)
Y = czas
xtx = t(X)%*%X
A = solve(xtx)%*%t(X)%*%Y
model =A[1]+ A[2]*lek + A[3]*sbp

wariancja = sum((czas-model)^2)/(NROW(model)-2-1)
odchylenie = sqrt(wariancja)

SSR = t(model-mean(Y))%*%(model-mean(Y))
SSE = (Y - model)%*%(Y-model)
SST = t(Y-mean(Y))%*%(Y-mean(Y))
R2 = SSR/SST
R2a = 1-((1-R2)*(NROW(model)-1)/(NROW(model)-2-1))

res = Y-model;
sum_res = sum(res)
mean_res = mean(res)

f = (SSR/2)/(SSE/(NROW(model)-2-1))



naglowek = c('A0', 'A lek', 'A sbp', 'SSR', 'SSE', 'SST', 'R2','R2a',  'sum res', 'avg res', 'wariancja', 'odchylenie', 'F')
wyniki = c(A[1], A[2], A[3], SSR, SSE, SST, R2, R2a,sum_res, mean_res, wariancja, odchylenie, f )
write.table(cbind(naglowek, wyniki),"",row.names = F)

plot(model,res, col = ifelse(abs(res) >(1*odchylenie),'red','green'))
text(model,res, labels=1:NROW(res),cex= 0.7, pos=2)


plot(Y,res, col = ifelse(abs(res) >(1*odchylenie),'red','green'))
text(Y,res, labels=1:NROW(res),cex= 0.7, pos=4)
