#####載入Cars93#####
install.packages('MASS')
library(MASS)
#View(Cars93)
#str(Cars93)
#head(Cars93)
#summary(Cars93)  

#####處理資料(只取出要調查的)#####
car = Cars93[ , c("Type" , "Price" , "Horsepower" , "RPM" , "Man.trans.avail" , "Fuel.tank.capacity" , "Weight" , "Origin")]
#View(car)
#summary(car) ##得知Price的mean = 19.51(x1000 USD) , Horsepower的mean = 143.8 , RPM的mean = 5281 etc.
#is.factor(car$Type)  ##確認是否為factor
#is.factor(car$Man.trans.avail)
#is.factor(car$Origin)

###把Price變factor###
#hist(Cars93$Price,xlab="Price (x 1.000 USD)",ylab="Frequency",xlim =c(0,70),main="Prices of Cars93",probability=TRUE)
#lines(density(Cars93$Price),col="blue")
f <- as.character(car$Price)
for (i in 1:length(car$Price)) {
  if (car$Price[i] < 13) f[i] <- as.character("low Price") 
  else if (13 <= car$Price[i] && car$Price[i] < 20) f[i] <- as.character("medium Price") 
  else if (20 <= car$Price[i] && car$Price[i] < 26)  f[i] <- as.character("high Price")
  else f[i] <- as.character("supreme Price")   #  if (26 <= car$Price[i]) 
}
car$Price <- as.factor(f)
table(as.factor(car$Price))

###把Horsepower變factor###
#hist(Cars93$Horsepower,xlab="Horsepower",ylab="Frequency",xlim =c(50,310),main="Horsepower of Cars93",probability=TRUE)
f <- as.character(car$Horsepower)
for (i in 1:length(car$Horsepower)) {
  if (car$Horsepower[i] < 103) f[i] <- as.character("low Horsepower") 
  else if (103 <= car$Horsepower[i] && car$Horsepower[i] < 170) f[i] <- as.character("medium Horsepower") 
  else f[i] <- as.character("high Horsepower")    
}
car$Horsepower <- as.factor(f)
table(as.factor(car$Horsepower))

###把RPM變factor###
#hist(Cars93$RPM,xlab="RPM",ylab="Frequency",xlim =c(3500,6500),main="RPM of Cars93",probability=TRUE)
f <- as.character(car$RPM)
for (i in 1:length(car$RPM)) {
  if (car$RPM[i] < 4800) f[i] <- as.character("low RPM") 
  else if (4800 <= car$RPM[i] && car$RPM[i] < 5750) f[i] <- as.character("medium RPM") 
  else f[i] <- as.character("high RPM")    
}
car$RPM <- as.factor(f)
table(as.factor(car$RPM))

###把Weight變factor###
#hist(Cars93$Weight,xlab="Weight",ylab="Frequency",xlim =c(1500,4500),main="Weight of Cars93",probability=TRUE)
f <- as.character(car$Weight)
for (i in 1:length(car$Weight)) {
  if (car$Weight[i] < 2620) f[i] <- as.character("light Weight") 
  else if (2620 <= car$Weight[i] && car$Weight[i] < 3525) f[i] <- as.character("normal Weight") 
  else f[i] <- as.character("heavy Weight")    
}
car$Weight <- as.factor(f)
table(as.factor(car$Weight))

###把Fuel.tank.capacity變factor###
#hist(Cars93$Fuel.tank.capacity,xlab="Fuel.tank.capacity",ylab="Frequency",xlim =c(5,30),main="Fuel.tank.capacity of Cars93",probability=TRUE)
f <- as.character(car$Fuel.tank.capacity)
for (i in 1:length(car$Fuel.tank.capacity)) {
  if (car$Fuel.tank.capacity[i] < 14.5) f[i] <- as.character("low Fuel.tank.capacity") 
  else if (14.5 <= car$Fuel.tank.capacity[i] && car$Fuel.tank.capacity[i] < 18) f[i] <- as.character("medium Fuel.tank.capacity") 
  else f[i] <- as.character("high Fuel.tank.capacity")    
}
car$Fuel.tank.capacity <- as.factor(f)
table(as.factor(car$Fuel.tank.capacity))

#str(car) ##data.frame

#####畫種類分布圖#####
#barplot(table(Cars93$Type),ylim = c(0,25),ylab="Frequency",axis.lty="solid",space=.20)
#pie(table(Cars93$Type))
#plot(Cars93$MPG.city ~ Cars93$Horsepower,
     #xlab="HP",ylab="Mileage", 
     #main ="Consumption Miles per Gallon vs. Power",pch=16) # pch=16 -> black points

#####把檔案寫入CSV#####
write.csv(car, "car.csv", row.names=F)
library(knitr)
kable(car[1:5,])

#####輸出CSV文件#####
install.packages('arules')
library(arules)
car_tran <- read.transactions("car.csv", sep = ",", skip = 1, rm.duplicates=TRUE) ##rm.duplicates表示刪除有重複的資料
summary(car_tran) ##transactions

#####inspect items#####
install.packages('tm')
library(tm)
inspect(car_tran[1:5,])

#####看car中的item出現次數多寡#####
install.packages('plotly')
library(plotly)
#itemFrequency(car[, 1:5])
itemFrequencyPlot(car_tran, topN=10) ##看前10名,可看出大多車都有手動變速箱(Yes),且差不多有一半是non-USA
#image(car)
#subset_int <- sample(nrow(car), 50 , replace = F) ##隨便抓50台車
#image(car[subset_int,]) ##可以看出某些性能比較多人選

#####訓練模型#####
#apriori(car) ##初始值 s = 0.1 , c = 0.8 
#car_rule <- apriori(car, parameter=list(support = 0.1, confidence = 0.8, minlen = 2)) ##此為data.frame資料(會顯示種類)
car_tran_rule <- apriori(car_tran, parameter=list(support = 0.1, confidence = 0.8, minlen = 2))
car_tran_rules <- apriori(car_tran,
                 parameter = list(minlen = 3, support = 0.05, confidence = 0.7),
                 appearance = list(rhs="supreme Price", default="lhs"), control = list(verbose=F)) ##讓rhs固定為supreme Price 降低s & c 來得到適合的rules
##支援度support，亦即X和Y同時出現的次數 ÷ 所有交易數 , 指同時擁有lhr & rhs特性的車中占全部車的比例
##信賴度confidence，亦即X和Y同時出現的次數 ÷ X出現的次數 , 指擁有lhs特性的車中也有rhs特性的比例

#car_rule
car_tran_rule ##595 rules
car_tran_rules ##56 rules
#inspect(car_rule[1:20]) ##lhs = left hand sides
inspect(car_tran_rule[1:15])
inspect(car_tran_rules[1:15])
#summary(car_rule) 
##lift = confidence/support(Y) (這裡的support是Y出現次數 ÷ 所有交易數,和上面的不同)
##lift在此研究中可解讀成是:在擁有lhs特性的情況下,價格是supreme Price的可能性有多大,所以lift越高越好
summary(car_tran_rule) ##lift的mean = 2.932
summary(car_tran_rules) ##lift的mean = 4.146

#####視覺化confidence&support#####
install.packages("arulesViz")
library(arulesViz)
plot(sort(car_tran_rules))

#####排序特定的值#####
#inspect(car_rule)
#inspect(sort(car_rule, by="lift")[1:15]) ##挑出lift最高的
inspect(sort(car_tran_rule, by="lift")[1:15])
inspect(sort(car_tran_rules, by="lift")[1:15])

#####刪除多餘的規則#####
rules_lift <- sort(car_tran_rules, by = 'lift')
rules_pruned <- rules_lift[!is.redundant(rules_lift, measure="lift")]
##is.redundant 意思是lhs的品項變多 但measure = 'lift'的值卻沒有大於品項少的 那就是多餘的
inspect(rules_pruned) ##從原本56個rules裁減到剩24個rules

#####找特定的子集#####
#fi_rules <- subset(car_tran_rules, items %in% "supreme Price")
#inspect(sort(fi_rules , by = 'lift')[1:20])

#####圖形化#####
install.packages('arulesViz')
library(arulesViz)
plot(sort(rules_pruned, by="lift"), method="grouped", control=list(type="items"), 
           main = "Grouped Matrix for the 14 Fentanyl-associated Rules")

plot(rules_pruned , method="graph" , alpha = 1 , control = list(type="items"))

#####將rules寫進CSV#####
write(rules_pruned, file = "carrule.csv", sep=",", row.names=F)




