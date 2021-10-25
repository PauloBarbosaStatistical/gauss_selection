dados<-read.csv2("C:/Users/Paulo Barbosa/Desktop/Gaus/Seleção que passei/athlete_events.csv",
                 header = TRUE,sep = ",",dec = ".")
boxplot(dados$Weight[dados$Sex=="M"]~dados$Year[dados$Sex=="M"])
options(scipen = 999) # Aparecer os números interiros
cbind(round(stat.desc(dados$Weight[dados$Sex=="M"]),2))

M_dados<-dados[dados$Sex=="M",]
F_dados<-dados[dados$Sex=="F",]
median(F_dados$Weight,na.rm=TRUE)
esta<-matrix(c(  "SummerF","SummerM",
                 mean(F_dados$Weight,na.rm = TRUE),
                 mean(M_dados$Weight,na.rm = TRUE),
                 median(F_dados$Weight,na.rm = TRUE),
                 median(M_dados$Weight,na.rm = TRUE),
                 sd(F_dados$Weight, na.rm = TRUE),
                 sd(M_dados$Weight, na.rm = TRUE),
                 min(F_dados$Weight, na.rm =TRUE),
                 min(M_dados$Weight, na.rm =TRUE),
                 max(F_dados$Weight, na.rm =TRUE),
                 max(M_dados$Weight, na.rm =TRUE)
                 
                 ),2,6)
require(xtable)
xtable(esta)

colnames(esta)<-c(" ","Médias","Medianas","Desvio Padrão", "Min", "Max")
quan_tis<-matrix(c("Mulher","Homem",
                 quantile(F_dados$Weight, probs = .25, na.rm = T),
                 quantile(M_dados$Weight, probs = .25, na.rm = T),
                 quantile(F_dados$Weight, probs = .75, na.rm = T),
                 quantile(M_dados$Weight, probs = .75, na.rm = T),
                 quantile(F_dados$Weight, probs = .95, na.rm = T),
                 quantile(M_dados$Weight, probs = .95, na.rm = T),
                 quantile(F_dados$Weight, probs = .99, na.rm = T),
                 quantile(M_dados$Weight, probs = .99, na.rm = T)),
                 2,5)
colnames(quan_tis)<-c("Quantis","25%","75%","95%","99%")

xtable(quan_tis)

boxplot(formula =Weight~Sex, data = dados, main = "Comparação entre atletas",
        col = "lightblue3")
legend(1.2,210,c("M-Masculino","F-Feminino"))


boxplot(formula = Weight~Year,
        data=M_dados, main = "Atletas Homens", col="lightblue2")

par(mfrow=c(1,2))
hist(M_dados$Weight, freq = F, col = "cadetblue3",
     main ="Homens",xlab = "Weight", )
curve(dnorm(x, mean(M_dados$Weight, na.rm = T),
            sd(M_dados$Weight, na.rm = T)),
            from=0,to=150, col = "black",add = T)

hist(F_dados$Weight, freq = F, col = "darkturquoise",
     main ="Mulheres",xlab = "Weight", )
curve(dnorm(x, mean(F_dados$Weight, na.rm = T),
            sd(F_dados$Weight, na.rm = T)),
      from=0,to=150, col = "black",add = T)

numerador<-mean(M_dados$Weight, na.rm = T) -  
            mean(F_dados$Weight, na.rm = T)
#....................................................#
#  Para obter as medidas descritivas basta usar o
#  pacote library(pastecs).
#  Depois stat.docs( ... ) entre parênteses coloque o
#  seu banco de dados.

library(pastecs)
stat.desc(dados)
# stat.anova() calcular a nova e escolher o teste a ser feito.

# Função cut(x, breaks = c(0,5,7,10))
# Serve para fazer uma distribuição de frequência com
# variáveis qualitativas.
# Nesse exemplo tem 4 classes.
# Quatidade de alunos dentro de cada classe.

#Plotar o histrograma de "x" e sua curva de densidade
#hist(x, dendity=T)
#lines(density(x))

a<-rnorm(1000)

hist(a,
     freq = F,
     main = "Variáveis normais",
     xlab = "Xi",
     ylab = "Densidade",
     col = "chocolate1")

lines(density(a))
curve(dnorm(x),-3,3, add = T, col = "black")

#.....................................................#
# Teste de normalidade dos dados

library(dplyr)
atletas <- dados %>% group_by(Sex)

atletas %>% count()
atletas %>% 
  group_by(Sex) %>% 
  summarise(media    = mean(Weight, na.rm = T),
            var_amos = sqrt(var (Weight, na.rm = T)))




tobs = (60-75.7)/(10.2/74522 + 13.2/196594)
