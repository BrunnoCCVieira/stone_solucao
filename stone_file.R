library(lubridate)
library(magrittr)
library(dplyr)
 


# A minha ideia é avaliar todas as segundas-feiras da primeira semana de janeiro de cada índice e prever o que acontecerá para as primeiras segundas-feiras da  
# primeira semana de janeiro dos anos seguintes - e assim sucessivamente. Se notou-se que em todas as segundas da primeira semana de janeiro de um índice houve
# publicação, em todas as próximas também haverá; se não hou em nenhuma das anteriores, o mesmo acontecerá com as posteriores.

# Quando há oscilação entre um ano e outro, a princípio tentei usar arima(3,0,0) - este modelo fez boa previsão quando usado o vetor c(1,0,1,1,0,0,1) por exemplo, 
# mas muito ruim quando obtinha vetores c(1,1,1,1,1,1,0,1), sendo 1 há publicação e 0 não há publicação. Isso me fez notar que minha estratégia de previsão não 
# abarcaria todos os casos em que algumas "segundas da primeira semana de janeiro" tem publicação e outras não.

# Para conseguir uma previsão a tempo, usei duas estratégias - que gostaria de ter feito diferente:  quando algumas "segundas da primeira semana de janeiro" apresentavam
# publicação e outras não, eu tirei uma probabilidade de ocorrer publicação e tracei uma linha de 80%, se a probabilidade de publicação está acima de 80% com base no últimos
# anos, nos próximos anos haverá publicações. Obviamente essa média móvel desconsidera as oscilações de um ano para o outro, o que é um ponto fraco do modelo.

# Comentei abaixo sobre um equivoco, retirei os dias 29, 30 e 31 devido ao problema que a quinta semana me gerava. Por isso fiz o looping j até 4. Eu teria que achar uma
# solução melhor para isso.

# Considero feriados, para cada índice, o dia de semana que nunca apresentou publicação.




# 1. Previsão, fim de semana e feriados -----------------------------------



name <- names(Indices_Publication_Dates_1_)

nova_serie <- data.frame(data = data.frame(seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "days")),
                         matrix(0, nrow =dim(data.frame(seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "days")))[1],
                                dim(Indices_Publication_Dates_1_)[2])) 

colnames(nova_serie) <- c("data", name)

nova_serie <- nova_serie %>% dplyr::mutate(
                                           ano = year(data),
                                           mes = month(data),
                                           dia = day(data),
                                           dia_semana = wday(data),
                                           semana_do_mes = ceiling(as.numeric(day(data))/7),
                                           data = as.character(data)) %>% 
  dplyr::filter(!(dia_semana %in% c(1, 7)), !(dia %in% c(29, 30, 31)))                         # Aqui estou cometendo um aquívoco que não consegui solucionar a tempo


fim_de_semana <- data.frame(data = data.frame(seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "days")),
                            matrix(0, nrow =dim(data.frame(seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "days")))[1],
                                   dim(Indices_Publication_Dates_1_)[2])) 

colnames(fim_de_semana) <- c("data", name)

feriado <- data.frame(data = data.frame(seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "days")),
                                matrix(0, nrow =dim(data.frame(seq(as.Date("2017-01-01"), as.Date("2023-12-01"), by = "days")))[1],
                                       dim(Indices_Publication_Dates_1_)[2])) 


colnames(feriado) <- c("data", name)



m <- 1

while ( m <= dim(Indices_Publication_Dates_1_)[2] ) {
  
# Tabela inicial + datas ausentes com peso 0 ------------------------------

ordem <- rbind(data.frame(data =  setdiff(as.character(seq(as.Date(min(dmy(Indices_Publication_Dates_1_[m][!is.na(Indices_Publication_Dates_1_[m])]))),
                           as.Date("2016-12-31"), by = "days")),
                           as.character(dmy(Indices_Publication_Dates_1_[m][!is.na(Indices_Publication_Dates_1_[m])]))), peso = 0),
                           data.frame(data = as.character(dmy(Indices_Publication_Dates_1_[m][!is.na(Indices_Publication_Dates_1_[m])])), peso = 1)) %>%
          dplyr::mutate(ano = year(data),
                        mes = month(data),
                        dia_semana = wday(data),
                        semana_do_mes = ceiling(as.numeric(day(data))/7)) %>%  
          dplyr::arrange(as.Date(data)) 



# Looping de inserção de dados filtrados da tabela 'ordem' para a tabela 'nova_serie' --------------------------------------------

i <- 1                                                # Parâmetro de mes
  j <- 1                                              # Prâmetro de semana
    k <- 1                                            # Parâmetro de dia
    
  while( i <= 12 ) {                                
    
    
    while ( j <= 4 ) {                                                                    
      
      
      while( k <= 7) {
        
        
        aux <- ordem %>% dplyr::select(ano, mes, dia_semana, semana_do_mes, peso) %>% dplyr::filter(mes == i, semana_do_mes == j, dia_semana == k )
        
        if ( mean(aux$peso) == 1 ) {     # Se todas as 'segundas' do mês de janeiro da primeira semana tem publicação, nos próximos anos a tendência seguirá
          
         nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), m + 1 ] <- 
            as.character(stringr::str_c(year(nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), 1]),
                                        "-", month(nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), 1]),
                                        "-", day(nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), 1])))
            
          
        } else if ( mean(aux$peso) == 0 ) {  # Se todas as 'terças' do mês de janeiro da primeira semana não tem publicação publicação, nos próximos anos a tendência seguirá
          
         nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), m + 1 ] <- 
           as.character("")
          
        } else { # Quando a tendência oscila, o modelo arima(3,0,0) se mostrou promissora ferramenta de previsão
          
          if( mean(aux$peso) >= 0.8 ) {
            
            nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), m + 1 ] <- 
              nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), m + 1 ] <- 
              as.character(stringr::str_c(year(nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), 1]),
                                          "-", month(nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), 1]),
                                          "-", day(nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), 1])))
            
          } else {
            
            nova_serie[(nova_serie$mes == i) & (nova_serie$semana_do_mes == j) & (nova_serie$dia_semana == k), m + 1 ] <- as.character("")
            
          }                                                                      
          
          
        }
        
        k <-  k + 1
        
      }
        
      k <-  1
      j <-   j + 1
      
      }
      
    j <-  1
    i <-  i + 1
      
      
  }
    i <-  1
    m <- m + 1
    
}

nova_serie <- nova_serie %>% dplyr::mutate(data = NULL, ano = NULL, mes = NULL, dia = NULL, dia_semana = NULL, semana_do_mes = NULL)


write.table(nova_serie, "C:/Users/brunn/Desktop/Análises/Análises/MG/Stone/nova_serie.txt", sep = " ", row.names = FALSE, col.names = TRUE, na = "NA")
    
 

# Construindo a tabela dos fins de semana e feriado ---------------------------------


m <- 1
n <- 1

while ( m <= dim( Indices_Publication_Dates_1_ )[2] )  {

while ( n <= dim(fim_de_semana)[1] ) {
  
  if( wday(fim_de_semana[ n  , 1 ], abbr = FALSE, label = TRUE) == "domingo" | wday(fim_de_semana[ n  , 1 ], abbr = FALSE, label = TRUE) == "sábado") { 
    
    fim_de_semana[ n, m + 1 ] <- 
    
    as.character(stringr::str_c(year(fim_de_semana[ n , 1] ),
                                "-", month(fim_de_semana[ n, 1 ]),
                                "-", day(fim_de_semana[ n , 1 ])))
    
    feriado[ n, m + 1 ] <- 
      
      as.character("nao")
  
} else {
  
  fim_de_semana[ n, m + 1 ] <- as.character("nao")
  
  
}
  n <- n + 1
}
  n <- 1
  m <- m + 1
}

fim_de_semana <- fim_de_semana %>% dplyr::mutate(data = NULL) %>% dplyr::filter( PX0001M != "nao")
feriado <- feriado %>% dplyr::filter( PX0001M != "nao")
write.table(fim_de_semana, "C:/Users/brunn/Desktop/Análises/Análises/MG/Stone/fim_de_semana.txt", sep = " ", row.names = FALSE, col.names = TRUE)



# Aqui vou buscar na base original os dias de semana que nunca tem publicação, esse dias serão os feriados para cada índice
# se 31/12 nunca teve publicação em PX0001M, esta data é feriado para esse índice --------


a <- 1   #Looping dos 40 índices
b <- 1   #Looping de 12 meses
c <- 1   #Looping dias do mês
contagem <- data.frame(matrix(0, nrow = 1, ncol = dim(Indices_Publication_Dates_1_)[2]))


while( a <= dim(Indices_Publication_Dates_1_)[2] ) {
  
  ordem1 <- rbind(data.frame(data =  setdiff(as.character(seq(as.Date(min(dmy(Indices_Publication_Dates_1_[a][!is.na(Indices_Publication_Dates_1_[a])]))),
                                                             as.Date("2016-12-31"), by = "days")),
                                            as.character(dmy(Indices_Publication_Dates_1_[a][!is.na(Indices_Publication_Dates_1_[a])]))), peso = 0),
                 data.frame(data = as.character(dmy(Indices_Publication_Dates_1_[a][!is.na(Indices_Publication_Dates_1_[a])])), peso = 1)) %>%
    dplyr::mutate(ano = year(data),
                  mes = month(data),
                  dia = day(data))
  
  while( b <= 12  ) {
    
    if( b %in% c(1, 3, 5, 7, 8, 10, 12) ) {
    
    while( c <= 31 ) {
      
      ordem2 <- ordem1 %>% dplyr::filter( mes == b, dia == c )
      
      if ( mean(ordem2$peso) == 0 ) {
        
        feriado[ day( feriado$data ) == c & month( feriado$data ) == b, a + 1 ] <- 
          as.character(stringr::str_c(year(feriado[ day( feriado$data ) == c & month( feriado$data ) == b , 1] ),
                                      "-", month(fim_de_semana[ day( feriado$data ) == c & month( feriado$data ) == b, 1 ]),
                                      "-", day(fim_de_semana[ day( feriado$data ) == c & month( feriado$data ) == b , 1 ])))
        
      } else {
        
        feriado[ day( feriado$data ) == c & month( feriado$data ) == b, a + 1 ] <- as.character("")
        
      }
    
      c <- c + 1
      
    }
      c <- 1
      
      } else if ( b %in% c(4, 6, 9, 11) ) {
      
      while (c <= 30) {
        
        ordem2 <- ordem1 %>% dplyr::filter( mes == b, dia == c )
        
        if ( mean(ordem2$peso) == 0 ) {
        
        feriado[ day( feriado$data ) == c & month( feriado$data ) == b, a + 1 ] <- 
          as.character(stringr::str_c(year(feriado[ day( feriado$data ) == c & month( feriado$data ) == b , 1] ),
                                      "-", month(fim_de_semana[ day( feriado$data ) == c & month( feriado$data ) == b, 1 ]),
                                      "-", day(fim_de_semana[ day( feriado$data ) == c & month( feriado$data ) == b , 1 ])))
        
        }else {
          
          feriado[ day( feriado$data ) == c & month( feriado$data ) == b, a + 1 ] <- as.character("")
          
        }
        
        c <- c + 1
        
      }
        
        c <- 1
      
      } else {
      
        while( c <= 29 ) {
          
          ordem2 <- ordem1 %>% dplyr::filter( mes == b, dia == c )
          
          if ( mean(ordem2$peso) == 0 ) {
            
            feriado[ day( feriado$data ) == c & month( feriado$data ) == b, a + 1 ] <- 
              as.character(stringr::str_c(year(feriado[ day( feriado$data ) == c & month( feriado$data ) == b , 1] ),
                                          "-", month(fim_de_semana[ day( feriado$data ) == c & month( feriado$data ) == b, 1 ]),
                                          "-", day(fim_de_semana[ day( feriado$data ) == c & month( feriado$data ) == b , 1 ])))
            
          }else {
            
            feriado[ day( feriado$data ) == c & month( feriado$data ) == b, a + 1 ] <- as.character("")
            
          }
          
          c <- c + 1
          
        }
        
        c <- 1
        
    }
    
    b <- b + 1
    
      
  }
  b <- 1 
  c <- 1
  contagem[a] <- dim(data.frame(feriado[a + 1][feriado[ a + 1] != "",]))[1] 
  a <- a + 1
  
  
  }


# Considerando que o vetor 'contagem' tem o número de dias que não apresentarão publicação e são dias de semana (feriado), farei um .txt com 
# o número de linhas do índice com maior número de feriados

feriado_menor_arquivo <- data.frame(matrix("", nrow = max(contagem), dim(Indices_Publication_Dates_1_)[2])) 

colnames(feriado_menor_arquivo) <- c( name )

r <- 1


while ( r <=  dim(Indices_Publication_Dates_1_)[2] ) {
  
    s <-  contagem[ r ]  
    
    feriado_menor_arquivo[ r ] <- matrix(c(feriado[ r + 1 ][feriado[ r + 1 ] != "",], rep("", max(contagem) - s )))
    
    r <- r + 1
    
  }
  
write.table(feriado_menor_arquivo , "C:/Users/brunn/Desktop/Análises/Análises/MG/Stone/feriado.txt", sep = " ", row.names = FALSE, col.names = TRUE) 




# 2. k-mean algorithm -----------------------------------------------------

# Aqui construirei uma matriz 40x11, sendo 40 os objetos PX0003M... e 11 as colunas com o variação média do mês de janeiro para fevereiro, de fevereiro para março,
# e assim sucessivamente das freências de ocorrência de publicação mensal

# 1. Problemas potenciais:

# A escolha inicial do centroide é uma adivinhação, o que pode reduzir a eficiência da separação

# 2. Estrutura:

# Usei k-mean para separar o salto médio mensal dos meses consecutivos em cada ano. Não usei algoritimos de bibliotecas para a realização do k-mean

# 3. O objetivo de trabalhar com o k-mean é minimizar a soma dos erros quadráticos. Este método emprega uma aproximação iterativa que minimiza o SSE de tal forma que 
# haja convergência para pontos ótimos locais ao invés de uma cluster ótima global.


matriz_salto <- data.frame(matrix(c(name, rep(0, 11*40)), nrow = 40, ncol = 12), stringsAsFactors = FALSE)
colnames(matriz_salto) <- c( "indice", "jan_fev", "fev_mar", "mar_abr", "abr_mai", "mai_jun", "jun_jul", "jul_ago", "ago_set", "set_out", "out_nov", "nov_dez" )


x <- 40          # Parâmetro dos 40 índices
y <- 1           # Parâmetro mensal
k <- 1           # Parâmetro auxiliar


while ( x <=  dim(Indices_Publication_Dates_1_)[2] ) {

  aux2 <- data.frame( matrix(0, nrow = 1, ncol = ( year(max((as.Date(max(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])])))))) + 1 -
                                                    year(min((as.Date(min(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])])))))))), stringsAsFactors = FALSE)
  
  ano_inicial <- year(min((as.Date(min(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])]))))))
  ano_final <- year(max((as.Date(max(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])]))))))

  while ( y <= 11 ) {
    
    
    
    
    while ( ano_inicial <= ano_final  )  {
   
 

      ordem2 <- rbind(data.frame(data =  setdiff(as.character(seq(as.Date(min(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])]))),
                                                                  as.Date("2016-12-31"), by = "days")),
                                                 as.character(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])]))), peso = 0),
                      data.frame(data = as.character(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])])), peso = 1)) %>% 
        dplyr::mutate(ano = year(data),
                      mes = month(data)) %>% 
        dplyr::filter( mes == y  , ano ==  ano_inicial) %>% 
        dplyr::summarize(freq = mean(peso))
      
      ordem3 <- rbind(data.frame(data =  setdiff(as.character(seq(as.Date(min(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])]))),
                                                                  as.Date("2016-12-31"), by = "days")),
                                                 as.character(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])]))), peso = 0),
                      data.frame(data = as.character(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])])), peso = 1)) %>% 
        dplyr::mutate(ano = year(data),
                      mes = month(data)) %>% 
        dplyr::filter( mes == y + 1 , ano ==  ano_inicial) %>% 
        dplyr::summarize(freq = mean(peso))

if ( (ordem3-ordem2)/ordem2 == "NaN") {
  
  aux2[k] <- 0
  
} else {

aux2[k] <- round((ordem3-ordem2)/ordem2, 5)

}

k <- k + 1

ano_inicial <- ano_inicial + 1  

    }
    
    ano_inicial <- year(min((as.Date(min(dmy(Indices_Publication_Dates_1_[x][!is.na(Indices_Publication_Dates_1_[x])]))))))
    
    k <- 1
    
    matriz_salto[ x , y + 1 ] <- round( mean( as.numeric(aux2)) , 5 ) 
    
    y <- y + 1
    
  }
  
  y <- 1
 
  x <- x + 1

}

write.table(matriz_salto, "matriz_cluster_1.txt", sep = " ", row.names = FALSE, col.names = TRUE )


# Obtida a matrix com as variações mensais médias, construirei o algoritmo k-mean para cada período. Quero classificar em k = 3 grupos,
# um limete de convergência e = 0.01 e centroides iniciais c(-0.2, 0, 0.2).

# "A" significa intervalo de menor salto em certo período: quando um objeto é classificado em A, ele está no grupo dos que apresentaram maior queda ou menor crescimento entre meses
# "B" significa o  intervalo intermediário de salto em certo período: quando um objeto é classificado em B, ele está no grupo dos que apresentaram queda ou crescimento intermediário
# "C" significa o intervalo de maior salto em certo período: quando um objeto é classificado em C, ele está no grupo dos que apresentaram maior aumento ou menor crescimento entre meses 



centroide <- data.frame(matrix(c(-0.020, 0.0, 0.02), nrow = 1, ncol = 3), stringsAsFactors = FALSE)
convergencia <- 3
aux1 <- data.frame(matrix(0, nrow = 1, ncol = 3 ), stringsAsFactors = FALSE)
aux2 <- data.frame(matrix(0, nrow = 40, ncol = 2 ), stringsAsFactors = FALSE)
names(aux2) <- c("cluster", "valor")
i <- 1
j <- 1
k <- 1
m <- 1
p <- 1



  
    
    while ( convergencia >= 0.05 ) {
    
    for ( i in c( 1 : 40 ) ) {
    
    for ( j in c( 1 : 3 ) ) {
      
      aux1[j] <- 1000*(matriz_salto[i, k + 1] - centroide[j])^2 
      
      j <- j + 1
      
    }
      
    if(aux1[1] == min(aux1)) {
           
             aux2[i,1] <-  "A"
             aux2[i,2] <-  matriz_salto[ i , k + 1]
             
    
    } else if ( aux1[2] == min(aux1) ) {
      
      aux2[i,1] <-  "B"
      aux2[i,2] <-  matriz_salto[ i , k + 1]
      
    } else {
      
      aux2[i,1] <-  "C"
      aux2[i,2] <-  matriz_salto[ i , k + 1]
      
    }
      
    j <- 1
    
    i <- i + 1
      
    }
    
    aux3 <- aux2 %>% dplyr::arrange(cluster) %>% group_by(cluster) %>% dplyr:: summarize(media = mean(valor))
    
    
    convergencia <- 10000*( (centroide[1] - aux3[1,2]) + (centroide[2] - aux3[2,2]) + (centroide[3] - aux3[3,2])  )^2  # Convergência é somatório||med(t) - med(t-1)||^2  
    
    for ( m in c(1 : 3) ) {
      
      centroide[m] <- aux3[m , 2]
      
      m <- m + 1
      
    }
    
    m <- 1
    
    i <- 1
    
    }
    
    
   
# Eu optei por não automatizar o preenchimento da tabela com os clusters porque, como devemos supor um calor inicial do centroide, um mesmo valor inicial de centroide 
# não se mostrou interessante na classificação de todos os 11 períodos. Em alguns momentos tive que avaliar "no olho" qual seria o melhor centroide inicial para cada
# coluna. As convergências ocorreram após entre 2 e 7 ciclos. Seria interessante avaliar as sequências de A,B,C para cada objeto ao longo do ano mas, devido ao prazo,
# não será possível.












































