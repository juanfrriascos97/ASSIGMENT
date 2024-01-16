
        t.test(CARGA ~ OBJETIVO, data = data)
wilcox.test (CARGA ~ OBJETIVO, data = data)

bp <- boxplot(CARGA ~ OBJETIVO, data = data, 
              xlab = "Bigeminy", ylab = "PVC Burden", 
              main = "PVC Burden VS Bigeminy, p= <0.001", outline = FALSE)

medians <- tapply(data$CARGA, data$OBJETIVO, median)

text(x = 1:length(medians), y = bp$stats[3, ], labels = round(medians, 2), pos = 3, col = "blue")


columnas_a_testear <- c("EDAD", "FC", "CARGA", "FEVI")
variable_dependiente <- "TV"
resultados_t_test <- list()
for (columna in columnas_a_testear) {
        formula <- as.formula(paste(columna, "~", variable_dependiente))
        t_test_result <- t.test(formula, data = data)
        resultados_t_test[[columna]] <- t_test_result
        }
print(resultados_t_test)

f <- table(data$VI, data$SANO)
f
summary(f)
ALL_ABSOLUTE <- sum(f[2, ])
ALL_RELATIVE <- sum(f[2, ])/sum(f)
NO_ABSOLUTE <- f[2,1]
NO_RELATIVE <- f[2,1]/sum(f[ ,1])
YES_ABSOLUTE <- f[2,2]
YES_RELATIVE <- f[2,2]/sum(f[ ,2])
ALL_ABSOLUTE
print(ALL_RELATIVE, digits = 4)
NO_ABSOLUTE
print(NO_RELATIVE, digits = 4)
YES_ABSOLUTE
print(YES_RELATIVE, digits = 4)

log1= glm(SANO~ EDAD + DM + BB + ANTIARR + POLY + DUPLETA, data= data, family=binomial)
summary(log1)
exp(log1$coefficients)
xcoeff <- summary(log1)$coefficients
Alow <- xcoeff[2] - 2*xcoeff[2,2] 
Ahigh <- xcoeff[2] + 2*xcoeff[2,2]
exp(Alow)
exp(Ahigh)
Blow <- xcoeff[3] - 2*xcoeff[3,2] 
Bhigh <- xcoeff[3] + 2*xcoeff[3,2]
exp(Blow)
exp(Bhigh)
Clow <- xcoeff[4] - 2*xcoeff[4,2] 
Chigh <- xcoeff[4] + 2*xcoeff[4,2]
exp(Clow)
exp(Chigh)
Dlow <- xcoeff[5] - 2*xcoeff[5,2] 
Dhigh <- xcoeff[5] + 2*xcoeff[5,2]
exp(Dlow)
exp(Dhigh)
Elow <- xcoeff[6] - 2*xcoeff[6,2] 
Ehigh <- xcoeff[6] + 2*xcoeff[6,2]
exp(Elow)
exp(Ehigh)
Flow <- xcoeff[7] - 2*xcoeff[7,2] 
Fhigh <- xcoeff[7] + 2*xcoeff[7,2]
exp(Flow)
exp(Fhigh)
