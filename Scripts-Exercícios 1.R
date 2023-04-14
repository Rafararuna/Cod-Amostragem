########################### Códigos ###########################

#Letra a)
N <- 424
N_1 <- 44    
N_2 <- 133  
N_3 <- 247 

#Letra b)
amostra <- read_excel("C:/Users/jgararuna/Downloads/amostragem_estratificada.xlsx")
n <- 40
n_1 <- n*(N_1/N)  #amostra do estrato 1 = 4
set.seed(5)
amostra_1 <- sample(seq(1:44),4) #9 30 39 12

n_2 <- n*(N_2/N)  #amostra do estrato 2 = 13
set.seed(6)
amostra_2 <- sample(seq(1:133),13) #81 124  35  50 105 126 122  97  64   8  80 112  12

n_3 <- n*(N_3/N)  #amostra do estrato 3 = 23
set.seed(7)
amostra_3 <- sample(seq(1:247),23) #245  98  29  18  60 192  82 234  40 110  41  55 182  23 106  20 130   3 226  73 146  67 225

#Letra c)
amostra$preco <- as.numeric(amostra$preco)
amostra_1 <- amostra %>% filter (amostra$'estrato' == "Capa Dura") #CAPA DURA
amostra_2 <- amostra %>% filter (amostra$'estrato' == "Capa Comum") #CAPA COMUM
amostra_3 <- amostra %>% filter (amostra$'estrato' == "Kindle") #KINDLE

#média amostral do estrato h:
yh_1 <- sum(amostra_1$'preco')/n_1
yh_2 <- sum(amostra_2$'preco')/n_2
yh_3 <- sum(amostra_3$'preco')/n_3

#estimador do total populacional:
t_str <- (N_1*yh_1) + (N_2*yh_2) + (N_3*yh_3) 

#estimador da média populacional:
y_str <- t_str/N #26.37225

#Variância amostral do estrato h:
var_1 <- var(amostra_1$preco)
var_2 <- var(amostra_2$preco)
var_3 <- var(amostra_3$preco)

var_str <- ((N_1/N)^2)*((N_1 - n_1)/N_1)*(var_1/n_1) + ((N_2/N)^2)*((N_2 - n_2)/N_2)*(var_2/n_2) + ((N_3/N)^2)*((N_3 - n_3)/N_3)*(var_3/n_3)

#intervalo de 95% de confiança:
IC_LEFT_p <-  y_str - (1.96*sqrt(var_str)) #21.29233
IC_RIGHT_p <- y_str + (1.96*sqrt(var_str)) #31.45217

#Letra d)
amostra$`numero de paginas` <- as.numeric(amostra$`numero de paginas`)
amostra_1 <- amostra %>% filter (amostra$'estrato' == "Capa Dura") #CAPA DURA
amostra_2 <- amostra %>% filter (amostra$'estrato' == "Capa Comum") #CAPA COMUM
amostra_3 <- amostra %>% filter (amostra$'estrato' == "Kindle") #KINDLE

#média amostral do estrato h:
yh_1 <- sum(amostra_1$'numero de paginas', na.rm = TRUE)/n_1
yh_2 <- sum(amostra_2$'numero de paginas', na.rm = TRUE)/n_2
yh_3 <- sum(amostra_3$'numero de paginas', na.rm = TRUE)/n_3

#estimador do total populacional:
t_str <- (N_1*yh_1) + (N_2*yh_2) + (N_3*yh_3) 

#estimador da média populacional:
y_str <- t_str/N #116.55

#Variância amostral do estrato h: 
var_1 <- var(amostra_1$'numero de paginas', na.rm = TRUE)
var_2 <- var(amostra_2$'numero de paginas', na.rm = TRUE)
var_3 <- var(amostra_3$'numero de paginas', na.rm = TRUE)

var_str <- ((N_1/N)^2)*((N_1 - n_1)/N_1)*(var_1/n_1) + ((N_2/N)^2)*((N_2 - n_2)/N_2)*(var_2/n_2) + ((N_3/N)^2)*((N_3 - n_3)/N_3)*(var_3/n_3)

#intervalo de 95% de confiança:
IC_LEFT_np <-  y_str - (1.96*sqrt(var_str)) #86.63978
IC_RIGHT_np <- y_str + (1.96*sqrt(var_str)) #146.4602

#Letra e)
amplitude_p_aas = 43.43299 - 24.08701 #19.34598
43.43299 - 33.76 = 9.67299
33.76 - 24.08701 = 9.67299

amplitude_np_aas = 183.9119 - 117.1085 #66.8034
183.9119 - 150.5102 = 33.4017
150.5102 - 117.1085 = 33.4017

amplitude_p_str = 31.45217 - 21.29233 #10.15984
31.45217 - 26.37225 = 5.07992
26.37225 - 21.29233 = 5.07992

amplitude_np_str = 146.4602 - 86.63978 #59.82042
146.4602 - 116.55 = 29.9102
116.55 - 86.63978 = 29.9102

#Letra f)
#em relação ao preço:
var_1p <- var(amostra_1$preco)
var_2p <- var(amostra_2$preco)
var_3p <- var(amostra_3$preco)

n_1p <- ((N_1*var_1p)/((N_1*var_1p)+(N_2*var_2p)+(N_3*var_3p)))*n #7.424862 = 7
n_2p <- ((N_2*var_2p)/((N_1*var_1p)+(N_2*var_2p)+(N_3*var_3p)))*n #13.68429 = 14
n_3p <- ((N_3*var_3p)/((N_1*var_1p)+(N_2*var_2p)+(N_3*var_3p)))*n #18.89084 = 19

#em relação ao número de páginas:
var_1np <- var(amostra_1$`numero de paginas`, na.rm = TRUE)
var_2np <- var(amostra_2$`numero de paginas`, na.rm = TRUE)
var_3np <- var(amostra_3$`numero de paginas`, na.rm = TRUE)

n_1np <- ((N_1*var_1np)/((N_1*var_1np)+(N_2*var_2np)+(N_3*var_3np)))*n #0.6602499 = 1
n_2np <- ((N_2*var_2np)/((N_1*var_1np)+(N_2*var_2np)+(N_3*var_3np)))*n #12.22208 = 12
n_3np <- ((N_3*var_3np)/((N_1*var_1np)+(N_2*var_2np)+(N_3*var_3np)))*n #27.11767 = 27