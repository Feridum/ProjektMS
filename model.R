# Projekt: Metody statystyczne INF Sem. 4 Gr. 2
# Temat: 3r - regresja wieloraka 
# Autorzy: Jarosław Bartoszuk, Mateusz Boral, Szymon Mendla, Mateusz Nieć, Aleksander Patschek

data = read.table("data.csv") #Dane pomiarowe importujemy z odpowiednio sformatowanego pliku.
lek = (data[,1])              #Pierwsza kolumna zawiera logarytm ilości leku w mg.
sbp = (data[,2])              #Druga kolumna odpowiada średniemu poziomowi ciśnienia w hipotensji.
czas = (data[,3])             #Trzecia odnosi się do czasu powrotu pacjenta do normy.

# Konstruowanie modelu
X = cbind(1,lek,sbp)                  #Tworzymy macierz zmiennych objaśniających poprzez złączenie kolumn - jedynkowej oraz dwóch zmiennych niezależnych 
Y = czas
xtx = t(X)%*%X                        #Funkcja t() tworzy macierz transponowaną; operator %*% służy do mnożenia macierzy.
A = solve(xtx)%*%t(X)%*%Y             #Funcka solve() rozwiązuje układ równań powstały na skutek operacji.
model = A[1] + A[2]*lek + A[3]*sbp    #Implementacja modelu regresjii wielorakiej (2.6.7.1 w podr.)


# Weryfikacja modelu
SSR = t(model-mean(Y))%*%(model-mean(Y))            #Sum of Square Regression - odchyleń wartości przewidywanych od średniej
SSE = t((Y - model))%*%(Y-model)                    #Sum of Square Errors - reszt
SST = t(Y-mean(Y))%*%(Y-mean(Y))                    #Sum of Square Total - odchyleń wartości obserwacji od średniej
R2 = SSR/SST                                        #Współczynnik determinacji wielorakiej
R2a = 1-((1-R2)*(NROW(model)-1)/(NROW(model)-2-1))  #Współczynnik dopasowania z uwzględnieniem ilości zmiennych i liczności prób.


wariancja = SSE/(NROW(model)-2-1)     #Funkcja NROW() zwraca ilość wierszy przekazanego obiektu.
odchylenie = sqrt(wariancja)

# Analiza reszt:
res = Y-model
sum_res = sum(res)
mean_res = mean(res)

# Test Kołmogorowa-Smirnowa zgodnie ze wzorem ze sprawozdania
dplus = c();
dminus = c();
dys_nor = pnorm(sort(res))                            #Funkcja pnorm() oblicza dystrybuantę.
n = NROW(res)
for(i in 1:n)
{
  dplus = append(dplus,abs(i/n-dys_nor[i]))           #Funkcja append() dopisuje wartość do wektora.
  dminus= append(dminus,abs(dys_nor[i]-(i-1)/n))
}

D = max(max(dplus),max(dminus))

# Test Durbina-Watsona zgodnie ze wzorem ze sprawozdania
licznik = 0;
for (t in 2:NROW(model))
{
  licznik = licznik+(res[t]-res[t-1])^2
}

dw = licznik/sum(res^2)

# Wypisywanie wyliczonych wartości:
naglowek = c('A0', 'A lek', 'A sbp', 'SSR', 'SSE', 'SST', 'R2','R2a',  'sum res', 'avg res', 'wariancja', 'odchylenie', 'dw','D')
wyniki = c(A[1], A[2], A[3], SSR, SSE, SST, R2, R2a,sum_res, mean_res, wariancja, odchylenie,dw,D )
write.table(cbind(naglowek, wyniki),"",row.names = F)

# Uzyskane wykresy:
plot(model,res, col = ifelse(abs(res) >(3*odchylenie[1,1]),'red','green'))
text(model,res, labels=1:NROW(res),cex= 0.7, pos=2)

plot(model,res^2, col = ifelse(abs(res) >(3*odchylenie[1,1]),'red','green'))
text(model,res^2, labels=1:NROW(res),cex= 0.7, pos=2)

plot(Y,res, col = ifelse(abs(res) >(3*odchylenie[1,1]),'red','green'))
text(Y,res, labels=1:NROW(res),cex= 0.7, pos=4)

plot(Y,res^2, col = ifelse(abs(res) >(3*odchylenie[1,1]),'red','green'))
text(Y,res^2, labels=1:NROW(res),cex= 0.7, pos=4)
