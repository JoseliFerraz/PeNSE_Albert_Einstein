
# Autor Joseli Moreira 

### VariÃ¡veis selecionadas
# 
# - RegiÃ£o
# - Sexo
# - Idade
# - Nos Ãºltimos 30 dias, em quantos dias fumou cigarro?
#   - Nos Ãºltimos 30 dias, em quantos dias tomou pelo menos um copo de bebida alcoÃ³lica?
#   - Alguma vez na vida, vocÃª usou alguma droga, tais como: maconha, cocaÃ�na, crack, cola, lolÃ³, lanÃ§a perfume, ecstasy, oxy, etc?


# Pacotes

require(tidyr,readr)

# ====================================================== #
#                 Ano 2012 
# ====================================================== #
# Variaveis de estudo

# Amostra 
# Fumou cigarro nos ultimos 30 dias B04003  
# rEGIAO  REGIAO 
# Sexo  B01001  
# Idade B01003 
# Atividade fisica B03005  
# Bebida alcoolica B05004  
# Usou alguma droga VB06001

# Banco principal
estudantes_2012 <- read_delim("estudantes.CSV")

names(estudantes_2012)
# ------------------------------------------------------------- # 
# Criando um banco para variaveis selecionadas neste estudo 
# ------------------------------------------------------------- # 
# Seelecao de 13 a 17 anos coluna B01003

estudantes_2012<- estudantes_2012[estudantes_2012$B01003==c(3,4,5,6,7),] #21192


Estudantes_2012_selecao<- data.frame(estudantes_2012$B04003,estudantes_2012$REGIAO,estudantes_2012$B01001,
                                     estudantes_2012$B01003,estudantes_2012$B03005,estudantes_2012$B05004,estudantes_2012$B06001)


names(Estudantes_2012_selecao)<- c("Fumou cigarro ulimos 30 dias","Regiao","Sexo","Idade","Atividade fisica","Bebida alcoolica ultimos 30 dias","Usou drogas")

# Removendo NA'S # Total 33
Estudantes_2012_selecao<- Estudantes_2012_selecao %>% drop_na()

write.csv2(Estudantes_2012_selecao,"Estudantes_2012_selecao.csv")
# Frequencia de idade
prop.table(table(Estudantes_2012_selecao$Idade))

# Sexo
table(Estudantes_2012_selecao$Sexo)

prop.table(table(Estudantes_2012_selecao$Sexo))


# Fumou cigarro nos ultimos 30 dias
Tabela_Fumante_2012<-table(Estudantes_2012_selecao$`Fumou cigarro ulimos 30 dias`)
names(Tabela_Fumante_2012)<-c("Nunca fumou","Nenhum","1 ou 2","3 a 5","6 a 9","10 a 19","20 a 29","Todos os dias","Nao Respondeu")
Tabela_Fumante_2012

prop.table(table(Estudantes_2012_selecao$`Fumou cigarro ulimos 30 dias`))
# Bebeu acool nos ultimos 30 dias
Tabela_acool_2012<-table(Estudantes_2012_selecao$`Bebida alcoolica ultimos 30 dias`)
names(Tabela_acool_2012)<-c("Nenhum","1 ou 2","3 a 5","6 a 9","10 a 19","20 a 29","Todos os dias","Nao Respondeu")
Tabela_acool_2012

# Jausou drogas
Tabela_drogas_2012<-table(Estudantes_2012_selecao$`Usou drogas`)
names(Tabela_drogas_2012)<-c("Sim","Nao","Nao Respondeu")
Tabela_drogas_2012
# -----------------------------------------------
#  Correlacao 
# -----------------------------------------------
cor(Estudantes_2012_selecao) # correlacao


# Tabela Regiao
Tab_2<- data.frame(table(Estudantes_2012_selecao$Regiao))


# ----------------------------------------------------------------------------------- #

# Genero x Consumo de drogas x Regiao
Data_1<- data.frame(Estudantes_2012_selecao$Regiao,Estudantes_2012_selecao$Sexo,Estudantes_2012_selecao$`Usou drogas`)
names(Data_1)<-c("Regiao","Sexo","Usou Drogas")

table(Data_1[Data_1$Regiao & Data_1$`Usou Drogas`==1,])

# Genero x Consumo de drogas x Regiao x idade
Data_2<- data.frame(Estudantes_2012_selecao$Regiao,Estudantes_2012_selecao$Sexo,Estudantes_2012_selecao$Idade,Estudantes_2012_selecao$`Usou drogas`)
names(Data_2)<-c("Regiao","Sexo","Idade","Usou Drogas")

table(Data_2[Data_2$Regiao & Data_2$`Usou Drogas`==1,])

table(Data_2$Regiao,Data_2$`Usou Drogas`)


# Genero x Bebida alcoolica nos ultimos 30 dias
Data_3<- data.frame(Estudantes_2012_selecao$Regiao,Estudantes_2012_selecao$Sexo,Estudantes_2012_selecao$Idade,Estudantes_2012_selecao$`Bebida alcoolica ultimos 30 dias`)
names(Data_3)<-c("Regiao","Sexo","Idade","Bebida alcoolica")





# ====================================================== #
#                 Ano 2015 : Amostra 1;2
# ====================================================== #
# Variaveis de estudo

# ------------------------------------------------------------------------# 
# -------------------------- #
# >>        AMOSTRA 1 # 
# -------------------------- #
# VB01003	-> Qual Ã© a sua idade?
# REGEOGR	-> RegiÃ£o geogrÃ¡fica
# VB01001	-> Qual Ã© o seu sexo?
# VB03006A -> atividade fisica nos ultimos 7 dias
# VB04003	-> NOS ÃLTIMOS 30 DIAS, em quantos dias vocÃª fumou cigarros?
# VB05004-> OnS ÃLTIMOS 30 DIAS bebida alcolica pelo menos 1 vez
# Usou alguma droga VB06001
# ------------------------------------------------------------------------# 

PENSE_AMOSTRA1_ALUNO <- read_delim("PENSE_AMOSTRA1_ALUNO.CSV")

# ------------------------------------------------ # -------------------------------------- #
# -------------------------- #
# >>      Amostra 2
# -------------------------- #
# rEGIAO  REGEOGR
# Sexo  VB01001  
# Idade vB01003 
# Atividade fisica VB03005A
# Fumou cigarro nos ultimos 30 dias VB04003  
# Bebida alcoolica VB05004  
# ------------------------------------------------ #
library(readr)
PENSE_AMOSTRA2_ALUNO <- read_delim("PENSE_AMOSTRA2_ALUNO.CSV")

PENSE_total_2015<- data.frame(c(PENSE_AMOSTRA1_ALUNO$VB05004,PENSE_AMOSTRA2_ALUNO$VB05004),c(PENSE_AMOSTRA2_ALUNO$REGEOGR,PENSE_AMOSTRA1_ALUNO$REGEOGR),
                              c(PENSE_AMOSTRA2_ALUNO$VB01001,PENSE_AMOSTRA1_ALUNO$VB01001),c(PENSE_AMOSTRA2_ALUNO$VB01003,PENSE_AMOSTRA1_ALUNO$VB01003),
                              c(PENSE_AMOSTRA2_ALUNO$VB03005A,PENSE_AMOSTRA1_ALUNO$VB03006A),c(PENSE_AMOSTRA2_ALUNO$VB05004,PENSE_AMOSTRA1_ALUNO$VB05004),
                              c(PENSE_AMOSTRA2_ALUNO$VB06001,PENSE_AMOSTRA1_ALUNO$VB06001))



names(PENSE_total_2015)<- c("Fumou cigarro ulimos 30 dias","Regiao","Sexo","Idade","Atividade fisica","Bebida alcoolica ultimos 30 dias","Usou drogas")


PENSE_total_2015<- PENSE_total_2015[PENSE_total_2015$Idade==c(13,14,15,16,17),]

# Removendo NA'S # T
PENSE_total_2015<- PENSE_total_2015 %>% drop_na()

write.csv2(PENSE_total_2015,"PENSE_total_2015.csv")
# Idade
prop.table(table(PENSE_total_2015$Idade))


# sexo
prop.table(table(PENSE_total_2015$Sexo))

# Fumou nos ultimos 30 dias
Tabela_Fumante_2015<-table(PENSE_total_2015$`Fumou cigarro ulimos 30 dias`)
names(Tabela_Fumante_2015)<-c("Pulou resposta","Nenhum","1 ou 2","3 a 5","6 a 9","10 a 19","20 a 29","Todos os dias","Nao Respondeu")
Tabela_Fumante_2015


prop.table(table(PENSE_total_2015$`Fumou cigarro ulimos 30 dias`))
# Bebeu acool nos ultimos 30 dias
Tabela_acool_2015<-table(PENSE_total_2015$`Bebida alcoolica ultimos 30 dias`)
names(Tabela_acool_2015)<-c("Nenhum","1 ou 2","3 a 5","6 a 9","10 a 19","20 a 29","Todos os dias","Nao Respondeu")
Tabela_acool_2015

#Usou drogas 
Tabela_drogas_2015<-table(PENSE_total_2015$`Usou drogas`)
names(Tabela_drogas_2015)<-c("Sim","Nao","Nao Respondeu")
Tabela_drogas_2015

# Jausou drogas
Tabela_drogas_2012<-table(Estudantes_2012_selecao$`Usou drogas`)
names(Tabela_drogas_2012)<-c("Sim","Nao","Nao Respondeu")
Tabela_drogas_2012
# -----------------------------------------------
#  Correlacao 
# -----------------------------------------------
cor(PENSE_total_2015) # correlacao

# Tabela Regiao
Tab_1<- data.frame(table(PENSE_total_2015$Regiao))


# ================T Grafico otal de aluno por regiao comparativo 2012 e 2015  ================

Alunos<- c(Tab_1$Freq,Tab_2$Freq)
Ano<- c(rep("2012",5),rep("2015",5))
Regiao<- rep(rep(c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste"),2))
dados<- data.frame(Alunos,Ano,Regiao)


graf1<- ggplot(data=dados,aes(x=Regiao,y=Alunos,fill=Ano))
graf1+geom_bar(stat="identity", width=0.5,position="dodge")+
  ggtitle("Totais de alunos por RegiÃ£o")+
  geom_text(aes(label=Alunos),position=position_dodge(width=0.8), vjust=-1)

