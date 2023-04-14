###############################################Lista 4##########################################################

############################################################################################

#Questao 55 

#a)

#calculando o estimador
x_barrau <- 50 #media pop da quantidade de empregados nas empresas
y_barra <- 5.2*10^6 #estimador da media de vendas por AAS
x_barra <- 45 #estimador da media de funcionarios por empresa por AAS
estimador <- (y_barra/x_barra)*x_barrau #5777778 = 5.7*10^6 =~ 5.8*10^6

#calculando o viés
n <- 100
N <- 10000
p <- 0.8
sigma_y <- 25*10^10
sigma_x <- 15
c <- (1/n)*(1-(n/N))*p*sigma_x*sigma_y #covariancia de xbarra e ybarra = 2.97*10^10
Vies <- (-c)/y_barra #-5711.538 = -5.711*10^3, portanto, tem-se um estimador viesado.

#b) 
var_estimada <- mean(y_barra - ((y_barra/x_barra)*(x_barrau)))^2 #variancia estimada para este estimador = 333827160494 =~ 0.333*10^12

#c)
ic_left <- estimador - (1.96*var_estimada) #-654295456790
ic_right <- estimador + (1.96*var_estimada) #6.5430710^11

###########################################################################################

#Questao 56
N <- 2010
N_1 <- 1580
N_2 <- 430
n <- 100
n_1 <- 70
n_2 <- 30
y_barra1 <- 19.40
y_barra2 <- 51.63
sigma_1 <- 312
sigma_2 <- 922

#a) 

estimador_pos <- sum(((N_1/N)*y_barra1),((N_2/N)*y_barra2)) #estimador da média populacional = 26.29498
  
#b)

#Distribuição:
#n1 segue uma ditribuição hipergeomtrica : n1 ~ hiper(M, N, n) -> n1 ~ hiper(N,n,N_1)

#média: n(M/N)
media_n1 <- n*(N_1/N) #78.60697

#variancia:
var_n1 <- n*((N-n)/(N-1))(N_1/N)((N-N_1)/N) #15.98773

#c) 
#estimador não viesado:
var_chapeu <- (1 - (n/N))(sum(((N_1/N)(sigma_1/n)), ((N_2/N)*(sigma_2/n)))) #4.204

#intervalo:
ic_left <- 26.30 - (1.96*var_chapeu) #18.05854
ic_right <- 26.30 + (1.96*var_chapeu) #34.54146

#######################################################################################

#Questao 4.08

library(SDaA)
agpop$N <- nrow(agpop)
head(agpop,n=10)
agsrsss <- agpop[sample(nrow(agpop), size=300, replace = F),]
nrow(agsrsss) # amostra de 300 observaçoes 

#a) plot the data
View(agsrsss)

#b) Estimador de regress?o para estimar o numero total de acres , com a variavel acres87, de 1992 usando o numero de 1987

N <- 3078
n <- 300
y_barra <- sum(agsrs$acres92)/n
x_barra <- sum(agsrs$farms87)/n
r <- y_barra/x_barra
tx <- 2087759
estimador <- r*tx #960155061 #total

#c)
cor_xy <- cor(agsrs$acres92, agsrs$farms87)
beta_chapeu <- (cor_xy*sd(agsrs$acres92))/sd(agsrs$farms87)
tot_farms <- (sum(agsrs$farms92, agsrs$farms87, agsrs$farms82)/n)
xu <- tx/3078
t_y_regressao <- y_barra + beta_chapeu*(xu - x_barra) #media
estimador <- t_y_regressao*3078 #total

#d)

#razao com farms87:
N <- 3078
n <- 300
y_barra <- sum(agsrs$acres92)/n
x_barra <- sum(agsrs$farms87)/n
r <- y_barra/x_barra
tx <- 2087759
estimador <- r*tx #960155061 #tot

#razao com acres87:
N <- 3078
n <- 300
y_barra <- sum(agsrs$acres92)/n
x_barra <- sum(agsrs$acres87)/n
r <- y_barra/x_barra
tx <- x_barra*N 
estimador <- r*tx #916927110

#regressao com farms 87:
cor_xy <- cor(agsrs$acres92, agsrs$farms87)
beta_chapeu <- (cor_xy*sd(agsrs$acres92))/sd(agsrs$farms87)
tot_farms <- (sum(agsrs$farms92, agsrs$farms87, agsrs$farms82)/n)
xu <- tx/3078
t_y_regressao <- y_barra + beta_chapeu*(xu - x_barra) #media = 299352.3
estimador <- t_y_regressao*N #total = 921406265


################################################################################

library(SDaA)
attach(agsrs)
str(agsrs)
dados = agsrs

# B
tx87 = 2087759
yb = sum(agsrs$acres92)/300
xb = sum(agsrs$farms87)/300

ty = tx87*(yb/xb)
ty

# SE(TY)

# C
sy = sqrt(var(agsrs$acres92))
sx = sqrt(var(agsrs$farms87))
p = cor(agsrs$farms87, agsrs$acres92)
xu = tx/3078
xb = sum(agsrs$farms87)/300
a = yb - b*(xb)
b = p*sy/sx
yreg = yb + b*(xu - xb)
yreg
teg = 3078*yreg
teg



###########################################################Lista 5#####################################################

#Questao 5.8

N -> 44
n -> 12
Mi -> quantidade de livros na estante selecionada =
vai selecionar aleatoriamente cinco livros entre 1-Mi

#a)
books <- read.csv("C:/Users/jgararuna/Downloads/books.csv")
books$shelf <- as.factor(books$shelf)

ggplot(books, aes(x= shelf, y= replace)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="PRATELERA", y="MEDIDA REPLACEMENT")+
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 
#medias nitidamente diferentes e variancias tambem.

#b)
estimador <- sum(((26/5)*48), ((52/5)*31), ((70/5)*46), ((47/5)*36), ((5/5)*209), ((28/5)*149), 
                       ((27/5)*259), ((29/5)*306), ((21/5)*252), ((31/5)*183), ((14/5)*271), ((27/5)*33)) #8901.2

erro_padrao <- sum(((1 - (5/26))*(26^2)*(var(c(13,13,3,8,11))/5)), ((1 - (5/52))*(52^2)*(var(c(6,7,8,5,5))/5)),
                   ((1 - (5/70))*(70^2)*(var(c(10,6,16,5,9))/5)), ((1 - (5/47))*(47^2)*(var(c(6,6,6,10,8))/5)),
                   ((1 - (5/5))*(5^2)*(var(c(8,7,5,68,121))/5)), ((1 - (5/28))*(28^2)*(var(c(16,7,7,60,59))/5)),
                   ((1 - (5/27))*(27^2)*(var(c(72,73,57,49,8))/5)), ((1 - (5/29))*(29^2)*(var(c(52,90,50,44,70))/5)),
                   ((1 - (5/21))*(21^2)*(var(c(53,60,50,59,30))/5)), ((1 - (5/31))*(31^2)*(var(c(19,5,65,41,53))/5)),
                   ((1 - (5/14))*(14^2)*(var(c(65,79,53,60,14))/5)), ((1 - (5/27))*(27^2)*(var(c(6,4,10,7,6))/5))) #372463.2


#c)
estimador <- (N/n)*sum(((26/5)*48), ((52/5)*31), ((70/5)*46), ((47/5)*36), ((5/5)*209), ((28/5)*149), 
                 ((27/5)*259), ((29/5)*306), ((21/5)*252), ((31/5)*183), ((14/5)*271), ((27/5)*33)) #91326.31

erro_padrao <- (N/n)*sum(((1 - (5/26))*(26^2)*(var(c(13,13,3,8,11))/5)), ((1 - (5/52))*(52^2)*(var(c(6,7,8,5,5))/5)),
                   ((1 - (5/70))*(70^2)*(var(c(10,6,16,5,9))/5)), ((1 - (5/47))*(47^2)*(var(c(6,6,6,10,8))/5)),
                   ((1 - (5/5))*(5^2)*(var(c(8,7,5,68,121))/5)), ((1 - (5/28))*(28^2)*(var(c(16,7,7,60,59))/5)),
                   ((1 - (5/27))*(27^2)*(var(c(72,73,57,49,8))/5)), ((1 - (5/29))*(29^2)*(var(c(52,90,50,44,70))/5)),
                   ((1 - (5/21))*(21^2)*(var(c(53,60,50,59,30))/5)), ((1 - (5/31))*(31^2)*(var(c(19,5,65,41,53))/5)),
                   ((1 - (5/14))*(14^2)*(var(c(65,79,53,60,14))/5)), ((1 - (5/27))*(27^2)*(var(c(6,4,10,7,6))/5))) #3821473

################################################################################################################

#Questao 5.10

#a)
one.way <- aov(replace~shelf, data = books)
summary(one.way)
#            Df   Sum Sq     Mean S    F value   Pr(>F)    
#shelf       11   25571      2324.6    4.759       6.58e-05 ***
#Residuals   48   23445      488.4

media <- mean(books$replace)
ssto <- (13-media)^2+(13-media)^2+(3-media)^2+ (8-media)^2+ (11-media)^2+
             (6-media)^2+ (7-media)^2+ (8-media)^2+ (5-media)^2+ (5-media)^2+ 
             (10-media)^2+ (6-media)^2+ (16-media)^2+ (5-media)^2+ (9-media)^2+
             (6-media)^2+ (6-media)^2+ (6-media)^2+ (10-media)^2+(8-media)^2+ (8-media)^2+
             (7-media)^2+ (5-media)^2+ (68-media)^2+ (121-media)^2+ (16-media)^2+ (7-media)^2+
             (7-media)^2+ (60-media)^2+ (59-media)^2+ (72-media)^2+ (73-media)^2+ (57-media)^2+
             (49-media)^2+ (8-media)^2+ (52-media)^2+ (90-media)^2+ (50-media)^2+ (44-media)^2+
             (70-media)^2+ (53-media)^2+ (60-media)^2+ (50-media)^2+ (59-media)^2+ (30-media)^2+
             (19-media)^2+ (5-media)^2+ (65-media)^2+ (41-media)^2+ (53-media)^2+ (65-media)^2+
             (79-media)^2+ (53-media)^2+ (60-media)^2+ (14-media)^2+ (6-media)^2+ (4-media)^2+ (10-media)^2+
             (7-media)^2+(6-media)^2 #49016.18

sigma_2 <- ssto/((N*M)-1) 
r_2a <- 1 -(2324.6/sigma_2) #-61.55378

#b) p<valor menor que alplha, ha evidencias para rejeitar h0, portanto nao sao similares.
#h0) medias iguais
#hq) medias diferentes

#c)
c1 <- 10
c2 <- 4
M <- 30
N <- 44
MSW <- 2324.6      
MSB <- 25571
mo <- sqrt((10*30*MSW)/(4*(MSB-MSW))) #2.738589 = 3

###################################################################################################

#Questao 5.12

coots <- read.csv("C:/Users/jgararuna/Downloads/coots.csv")


y_u <- sum(177.30, 93.25,116.29,104.63,17.50,63.00)/6 #95.32833
ssb <- sum((177.30-y_u)^2 , (93.25-y_u)^2,(116.29-y_u)^2,(104.63-y_u)^2,(17.50-y_u)^2,(63.00-y_u)^2) #14351.96
msb <- ssb/(45-1)

M_52 <- 177.30
SSW_52 <- sum((146-M_52)^2, (180-M_52)^2, (251-M_52)^2, (152-M_52)^2, (72-M_52)^2, (181-M_52)^2, (171-M_52)^2, (361-M_52)^2,
               (73-M_52)^2, (186-M_52)^2)

M_19 <- 93.25
SSW_19 <- sum((99-M_19)^2,(101-M_19)^2, (52-M_19)^2, (121-M_19)^2 )

M_37 <- 116.29
SSW_37 <- sum((199-M_37)^2, (179-M_37)^2, (98-M_37)^2, (63-M_37)^2, (126-M_37)^2, (87-M_37)^2, (62-M_37)^2)

M_39 <- 104.63
SSW_39 <- sum((226-M_39)^2, (129-M_39)^2, (57-M_39)^2, (46-M_39)^2, (86-M_39)^2, (43-M_39)^2, (85-M_39)^2, (165-M_39)^2)

M_8 <- 17.5
SSW_8 <- sum((12-M_8)^2, (23-M_8)^2)

M_14 <- 63
SSW_14 <- sum((87-M_14)^2, (43-M_14)^2, (59-M_14)^2)
SSW <- sum(SSW_52, SSW_19, SSW_37, SSW_39, SSW_8, SSW_14)

MSW <- SSW/(45*(169-1))

estimador <- sum(((52/6)*1773), ((19/6)*373), ((37/6)*814), ((39/6)*863), ((8/6)*35), ((14/6)*189))
                 
