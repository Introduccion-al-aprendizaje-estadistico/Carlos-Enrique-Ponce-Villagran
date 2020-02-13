Auto<-read.csv("Auto.csv")

########################## Inciso (b) ############################

range(Auto$mpg)

range(Auto$cylinders)

range(Auto$displacement)

range(Auto$horsepower)

range(Auto$weight)

range(Auto$acceleration)

range(Auto$year)

############################# Inciso (c) ##############################

mean(Auto$mpg)

mean(Auto$cylinders)

mean(Auto$displacement)

mean(Auto$horsepower)

mean(Auto$weight)

mean(Auto$acceleration)

mean(Auto$year)

sd(Auto$mpg)

sd(Auto$cylinders)

sd(Auto$displacement)

sd(Auto$horsepower)

sd(Auto$weight)

sd(Auto$acceleration)

sd(Auto$year)

############################Inciso (d)##########################

Auto<-Auto[-(10:85),]

range(Auto$mpg)

range(Auto$cylinders)

range(Auto$displacement)

range(Auto$horsepower)

range(Auto$weight)

range(Auto$acceleration)

range(Auto$year)

mean(Auto$mpg)

mean(Auto$cylinders)

mean(Auto$displacement)

mean(Auto$horsepower)

mean(Auto$weight)

mean(Auto$acceleration)

mean(Auto$year)

sd(Auto$mpg)

sd(Auto$cylinders)

sd(Auto$displacement)

sd(Auto$horsepower)

sd(Auto$weight)

sd(Auto$acceleration)

sd(Auto$year)

###########################Inciso (e)##############################

Auto<-read.csv("Auto.csv")

plot(Auto)

plot(Auto$weight,Auto$mpg,xlab = "Peso",ylab = "MPG",main = "Peso vs. MPG")

plot(Auto$weight,Auto$displacement,xlab = "Peso",ylab = "Desplazamiento", main = "Peso vs. Desplazamiento")

plot(Auto$mpg,Auto$year,xlab = "MPG",ylab = "Año",main = "MPG vs. Año")

############################# Inciso (f) #####################

plot(Auto$mpg,Auto$name,xlab = "MPG", ylab = "Nombres", main = "MPG vs. Nombres")
