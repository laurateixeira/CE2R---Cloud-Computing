################### Simulação populacional - Computação em nuvem CE2R ##########################

# Carregando pacotes necessários
library(tidyverse)
library(microbenchmark) #Caso queira medir os tempos


## Antes de começar vamos explicar o raciocínio inicial utilizado:

# a função lista_geracoes gera matrizes, onde cada matriz descreve uma geração, a primeira coluna
# é o casal pai do indivíduo, cada casal começa com um número que descreve de qual geração é esse casal, e depois do ponto vem apenas uma contagem de quantos casais temos
# já a segunda coluna é o indivíduo em si, onde 1 = homem e 2 = mulher
# na terceira coluna estão os casais formados nessa geração, o formato segue o que já foi descrito anteriormente.

############## FUNÇÕES ##################


lista_geracoes<- function(pop_inicial,geracoes,lambda=2.5){
#n.1
n.1<- matrix(data=NA,pop_inicial,2)
n.1[,1]<- sample(1:2,pop_inicial,replace = T)
casal.1<- paste0("1.",1:min(as.numeric(table(n.1[,1]))))
### Sorteando os casais 
if ((as.numeric(table(n.1[,1]))[1])>=(as.numeric(table(n.1[,1]))[2])){
  n.1[n.1[,1]==2,2] <- sample(casal.1)   
  x<-logical(length = length(n.1[,1]))
  x[sample(which(n.1[,1]==1),length(casal.1))]<- TRUE
  n.1[x,2] <- sample(casal.1) 
} else {
  n.1[n.1[,1]==1,2] <- sample(casal.1)   
  x<-logical(length = length(n.1[,1]))
  x[sample(which(n.1[,1]==2),length(casal.1))]<- TRUE
  n.1[x,2] <- sample(casal.1) 
}
lista<- list()
lista_casais <- list()
lista_casais[[1]]<- casal.1
### Criando as gerações
for (w in 1:geracoes){
  vetor_pais<- character()
  for (i in 1:(length(lista_casais[[w]]))){
    vetor_pais<- c(vetor_pais, rep(lista_casais[[w]][i],rpois(1,lambda)))
  }
  lista[[w]]<-matrix(data=NA,length(vetor_pais),3)
  lista[[w]][,1]<- vetor_pais
  lista[[w]][,2]<-sample(1:2,length(vetor_pais),replace = T)
  ## Criando os casais da geração
  ## Evite escrever seu código desse jeito:
  #eval(parse(text=paste0('casal.',w+1,'<-paste0(w+1,".",1:min(as.numeric(table(lista[[w]][,2]))))')))
  #assign(paste0("casal.",w+1), paste0(w+1,".",1:min(as.numeric(table(lista[[w]][,2])))))
  lista_casais[[w+1]]<-paste0(w+1,".",1:min(as.numeric(table(lista[[w]][,2]))))
  
  ### Sorteando os casais 
  if ((as.numeric(table(lista[[w]][,2]))[1])>=(as.numeric(table(lista[[w]][,2]))[2])){
    lista[[w]][lista[[w]][,2]==2,3] <- sample(lista_casais[[w+1]]) 
    x<-logical(length = length(lista[[w]][,2])) 
    x[sample(which(lista[[w]][,2]==1),length(lista_casais[[w+1]]))]<- TRUE
    lista[[w]][x,3] <- sample(lista_casais[[w+1]]) 
  } else {
    lista[[w]][lista[[w]][,2]==1,3] <- sample(lista_casais[[w+1]]) 
    x<-logical(length = length(lista[[w]][,2])) 
    x[sample(which(lista[[w]][,2]==2),length(lista_casais[[w+1]]))]<- TRUE
    lista[[w]][x,3] <- sample(lista_casais[[w+1]]) 
  }
}

return(lista)

}

########### Antepassados
antepassados<- function(x,lista){
  tamanho_lista<- length(lista)
  arvore<-list()
  arvore[[1]]<-x[1]
  vetor<-c()
    
    for (k in 2:(length(lista))){
      
      for (h in 1:length(arvore[[k-1]]))  { 
        
        if (h==length(arvore[[k-1]])){
          vetor <-  c(vetor,na.omit(lista[[(tamanho_lista+1)-k]][lista[[(tamanho_lista+1)-k]][,3]==arvore[[k-1]][h],1]))
          arvore[[k]]<- vetor
          vetor<-c()
          
        } else {
          vetor <-  c(vetor,na.omit(lista[[(tamanho_lista+1)-k]][lista[[(tamanho_lista+1)-k]][,3]==arvore[[k-1]][h],1]))}
        
        
      }
    }
  
  return(unlist(arvore))
  
}


calcular_redundancias<- function(qual_geracao,df_final){
  
df_final<- as.matrix(df_final)
   
index<-str_detect(df_final[1,],paste0(qual_geracao,"\\."))
  
matriz<-df_final[,index]

matriz<-unique(matriz) #Removendo os irmãos

p<-t(apply(matriz,1,function(i) duplicated(i))) #Identificando redundancias nas linhas

matriz[p]<- NA

repet<-as.numeric(table(matriz))

redundancia<- (sum(repet[repet!=1])/(dim(matriz)[1]*dim(matriz)[2])) *100 

return(redundancia)

}


### Fazendo a simulação rodar m vezes

lista_final <- list()
lista_intermed <- list() 

for (m in 1:5){
  
  lista_intermed[[m]]<- lista_geracoes(64000,3,3.5) # Aqui defina a pop inicial,quantidade de gerações e lambda (média de filhos)
  b<-lapply(lista_intermed[[m]][[length(lista_intermed[[m]])]],function(x) antepassados(x,lista_intermed[[m]]))
  b<-b[1:(length(b)/3)]
  
  df_final<- data.frame(b)
  df_final<-data.frame(t(df_final),row.names = NULL)
  
  lista_final[[m]] <-calcular_redundancias(2,df_final = df_final) #Calculando redundâncias para a 2ª geração
  
  
  
  
  
  
}

## O resultado desse laço é o valor da redundância em todas as vezes que a simulação rodou

## Mas caso queira você também pode testar as funções isoladamente

