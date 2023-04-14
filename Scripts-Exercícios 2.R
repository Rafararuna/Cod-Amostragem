########################################## Códigos ##################################################

# Questao 1
set.seed(2)
sample(seq(1:373), 50)

x <- c(1,2)
sample(x, 1)

library(readxl)
amostra <- read_excel("C:/Users/jgararuna/Downloads/Amostra.xlsx")

amostra$Edicao <- as.factor(amostra$Edicao)
Fr<-table(amostra$Edicao)
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=Var1, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#A11D21", width = 0.5) +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Edição", y="Frequência") +
  scale_y_discrete(limits = c(seq(0,60,10))) +
  expand_limits(y = c(0,60)) +
  theme_bw() 


amostra$Preco <- as.numeric(amostra$Preco)
summary(amostra$Preco)
sd(amostra$Preco)
ggplot(amostra, aes(x= "", y= Preco)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Preço")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

summary(amostra$`Numero de paginas`)
sd(amostra$`Numero de paginas`, na.rm = TRUE)
ggplot(amostra, aes(x= "", y= `Numero de paginas`)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Número de páginas")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black"))

amostra$Formato <- as.factor(amostra$Formato)
Fr<-table(amostra$Formato)
Pr<-as.data.frame(round(prop.table(Fr), digits=4)*100)
colnames(Pr)<-c("Var1", "Pr")
comp<-merge(Fr, Pr, by="Var1")
comp$Pr<-paste(gsub("\\.",",",comp$Pr), "%", sep= '')

ggplot(comp, aes(x=Var1, y=Freq, label=Pr)) +
  geom_bar(stat="identity", fill="#A11D21", width = 0.5) +
  geom_text(vjust=-0.5, size=4)+
  labs(x="Formato", y="Frequência") +
  scale_y_discrete(limits = c(seq(0,60,10))) +
  expand_limits(y = c(0,60)) +
  theme_bw()

amostra$Preco <- as.numeric(amostra$Preco)
soma_1 <- sum(amostra$Preco)
n_1 <- 50
media_1 <- soma_1/n_1 #33.76

N_1 <- 373
v_1 <- var(amostra$Preco) #1406.325
IC.LEFT_1 <- media_1 - (1.96*sqrt(1-(n_1/N_1))*((sqrt(v_1))/(sqrt(n_1)))) #24.08701
IC.RIGHT_1 <- media_1 + (1.96*sqrt(1-(n_1/N_1))*((sqrt(v_1))/(sqrt(n_1)))) #43.43299

soma_1.2 <- sum(amostra$`Numero de paginas`, na.rm = TRUE)
media_1.2 <- soma_1.2/(n_1 -1) #150.5102

v_1.2 <- var(amostra$`Numero de paginas`, na.rm = TRUE) #16768.8
IC.LEFT_1.2 <- media_1.2 - (1.96*sqrt(1-(n_1/N_1))*((sqrt(v_1.2))/(sqrt(n_1)))) #117.1085
IC.RIGHT_1.2 <- media_1.2 + (1.96*sqrt(1-(n_1/N_1))*((sqrt(v_1.2))/(sqrt(n_1)))) #183.9119

# Questao 2
N_2 <- 90000
N1_2 <- 35000
N2_2 <- 45000
N3_2 <- 10000
n_2 <- 900

#estrato 1:
n1_2 <- ((N1_2)/(N1_2 + (N2_2/2) + (N3_2/2)))*n_2 #504

#estrato 2:
n2_2 <- ((N2_2/2)/(N1_2 + (N2_2/2) + (N3_2/2)))*n_2 #324

#estrato 3:
n3_2 <- ((N3_2/2)/(N1_2 + (N2_2/2) + (N3_2/2)))*n_2 #72

n1 <- n_2*(N1_2/N_2) #350
n2 <- n_2*(N2_2/N_2) #450
n3 <- n_2*(N3_2/N_2) #100

#variância proporcional da amostragem estratificada
v.prop_str <- ((N1_2/N_2)^2)*((0.45*(1-0.45))/(n1)) + ((N2_2/N_2)^2)*((0.25*(1-0.25))/(n2)) + ((N3_2/N_2)^2)*((0.03*(1-0.03))/(n3)) #0.0002147037

#variância da amostra aleatória simples
parametro1_2 <- (N1_2/N_2)*0.45
parametro2_2 <- (N2_2/N_2)*0.25
parametro3_2 <- (N3_2/N_2)*0.03
p_chapeu <- sum(parametro1_2,parametro2_2,parametro3_2) #0.3033333
v.srs <- ((p_chapeu*(1 - p_chapeu))/(n_2)) #0.0002348025

p <- v.prop_str/v.srs  #0.9144014

# Questao 3
N_3 <- 100
n_3 <- 30
peso_3 <- N_3/n_3 #3.33

amostra_3p <- c(8*peso_3,5*peso_3,2*peso_3,6*peso_3,6*peso_3,3*peso_3,8*peso_3,6*peso_3,10*peso_3,7*peso_3,15*peso_3,9*peso_3,15*peso_3,3*peso_3,5*peso_3,6*peso_3,7*peso_3,10*peso_3,14*peso_3,3*peso_3,4*peso_3,5*peso_3,10*peso_3,6*peso_3,14*peso_3,12*peso_3,7*peso_3,8*peso_3,12*peso_3,9*peso_3)
soma_3 <- sum(amostra_3p)
media_3 <- soma_3/(n_3*peso_3) #7.833333

amostra_3p <- c(8*peso_3,5*peso_3,2*peso_3,6*peso_3,6*peso_3,3*peso_3,8*peso_3,6*peso_3,10*peso_3,7*peso_3,15*peso_3,9*peso_3,15*peso_3,3*peso_3,5*peso_3,6*peso_3,7*peso_3,10*peso_3,14*peso_3,3*peso_3,4*peso_3,5*peso_3,10*peso_3,6*peso_3,14*peso_3,12*peso_3,7*peso_3,8*peso_3,12*peso_3,9*peso_3)
total_pop <- sum(amostra_3p) #783.3333

amostra_3 <- c(8,5,2,6,6,3,8,6,10,7,15,9,15,3,5,6,7,10,14,3,4,5,10,6,14,12,7,8,12,9)
v_3 <- var(amostra_3)
v_total_pop <- (N_3^2)*(1-(n_3/N_3))*(v_3/n_3) #3155.364
sd_3 <- sqrt(v_total_pop) #56.17263
IC.LEFT_3 <- total_pop - (2.045*sd_3) #668.4603
IC.RIGHT_3 <- total_pop + (2.045*sd_3) #898.2063

ERRO.LEFT_3 <- total_pop - IC.LEFT_3 #114.873
ERRO.RIGHT_3 <- IC.RIGHT_3 - total_pop #114.873
margem_erro <- (114.8731/total_pop)*100 #14.66465%

# Questao 4
n_4 <- 400
IC.LEFT_4 <- 0.5 - (1.96*sqrt(((1 - 0.5)*0.5)/n_4)) #0.451
IC.RIGHT_4 <- 0.5 + (1.96*sqrt(((1 - 0.5)*0.5)/n_4)) #0.549

N_4 <- 2000
n_4_1 <- (N_4*(1.65^2)*(1-0.5))/(((N_4-1))*(0.02^2)+((1.65^2)*0.5*(1-0.5))) #1839.247

n_4_2 <- (N_4*(1.96^2)*(1-0.5))/(((N_4-1))*(0.03^2)+((1.96^2)*0.5*(1-0.5))) #1392.136



