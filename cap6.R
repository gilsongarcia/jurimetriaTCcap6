library(readxl)
library(tidyverse)
library(writexl)

r<-tcesp2


write_xlsx(r,"r.xlsx")
write_xlsx(relator,"relator.xlsx")

r2<-r
r<-r_relator

r<-tcesp2
table(r$Relator)
table(r$`Tipo de processo`)
r %>% count(Relator)

r %>% count(Relator)
r %>% count(Relator, sort=TRUE)
r %>% count(`Tipo de processo`)
r %>% count(`Tipo de processo`,sort=TRUE)
r %>% count(Entidade, sort = TRUE)
r%>% count(`Unidade Técnica`,sort=TRUE)
relator<-as.data.frame(table(r$Relator))
View(relator)
tipo<-as.data.frame(table(r$`Tipo de processo`))
tipo<-as.data.frame(table(r$`Tipo de processo`))
View(tipo)
entidade<-as.data.frame(table(r$Entidade))
View(entidade)
unidade<-as.data.frame(table(r$`Unidade Técnica`))
View(unidade)
r$ministerio<-str_detect(r$Entidade,"(?i)(ministério da saúde)")
View(r)
table(r$ministerio)
r$fundo<-str_detect(r$Entidade,"(?i)(fundo)")
table(r$fundo)
r$fundo<-str_detect(r$Entidade,"(?i)(fundo nacional)")
table(r$fundo)
r$fundacao<-str_detect(r$Entidade, "(?i)(fundação nacional")
r$fundacao<-str_detect(r$Entidade, "(?i)(fundação nacional)")
table(r$fundacao)
r$secretaria_estado<-str_detect(r$Entidade, "(?i)(secretaria estadual|
secretaria do estado|
secretaria de estado|secretaria de saúde do estado|secretaria da saúde do estado)")
table(r$secretaria_estado)
r$secretaria<-str_detect(r$Entidade, "(?i)(secretaria )")
table(r$secretaria)
r$secretaria_estado<-NULL
r$multa<-str_detect(r$Sumário, "(?i)(multa)")
table(r$multa)
r<-r %>% mutate(Ano_julgado = str_sub(Data,7,10))
r<-r %>% mutate(Ano_processo=str_sub(Processo,9,12))
r<-r %>% mutate(Ano_julgado=as.integer((Ano_julgado)))
r<-r%>% mutate(Ano_processo=as.integer(Ano_processo))
r<-r %>% mutate(Ano_processo=as.integer(Ano_processo))
r<-r %>% mutate(Duracao=Ano_julgado-Ano_processo)
r<-r %>% mutate(Duracao=as.integer(Duracao))
r<-r %>% mutate(mp=case_when(str_detect
                             (r$`Representante do Minist?rio P?blico`,
                               "n?o atuou|N?o atuou|n?o atuou.|N?o atuou.|n?o atuou")~
                               "nao atuou",TRUE~"atuou"))
r<-r %>% mutate(mp=case_when(str_detect
                             (r$`Representante do Minist?rio P?blico`,
                               "n?o atuou|N?o atuou|n?o atuou.|N?o atuou.|n?o h?")~
                               "nao atuou",TRUE~"atuou"))

table(r$mp)

r <- r %>% mutate(decisao=case_when(str_detect(decisao,
                                               "(?i)(multa)")~"1",TRUE~"0"))
r <- r %>% mutate(decisao=case_when(str_detect(multa,
                                               "(?i)(multa)")~"1",TRUE~"0"))
r$multa<-str_detect(r$Sumário, "(?i)(multa)")
r <- r %>% mutate(decisao=case_when(str_detect(multa,
                                               "(?i)(TRUE)")~"1",TRUE~"0"))
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)
r<-r %>% mutate(decisao=as.integer(decisao))
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)
summary(glm.fit)
coef(glm.fit)
exp(coef(glm.fit))
lm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",4051)
glm.pred[glm.probs >.5]="1"
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",4051)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
mean(r$decisao)
glm.fit=glm(decisao~
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",4051)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
glm.fit=glm(decisao~r$Relator
            r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",4051)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
table(r$Relator)
r %>% count(Relator, sort=TRUE)
View(relator)
View(tipo)
r1<-r
r <- r %>% group_by(relator) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Relator) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
table(r$Relator)
r <- r %>% group_by(r$`Tipo de processo`) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
relator<-as.data.frame(table(r$Relator))
tipo<-as.data.frame(table(r$`Tipo de processo`))
View(relator)
View(tipo)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",4051)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",4010)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
relator<-as.data.frame(table(r$Relator, sort=TRUE))
relator<-as.data.frame(table(r$Relator, sort=TRUE))
r %>% count(Entidade, sort = TRUE)
r%>% count(`Unidade Técnica`,sort=TRUE)
table(r$Ano_processo)
r <- r %>% group_by(r$Ano_processo) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
table(r$Ano_processo)
table(r$Ano_julgado)
r <- r %>% group_by(r$Ano_julgado) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
table(r$Ano_julgado)

glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)

summary(glm.fit)
table(r$Duracao)
View(tipo)
View(relator)

glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3970)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
r <- r %>% group_by(r$`Tipo de processo`) %>%
  filter(n()>20) %>%
  ungroup() %>%
  droplevels()
r<-r1
r <- r %>% group_by(r$Relator) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$`Tipo de processo`) %>%
  filter(n()>20) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_processo) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_julgado) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
View(relator)
relator<-as.data.frame(table(r$Relator))
tipo<-as.data.frame(table(r$`Tipo de processo`))
View(relator)
View(relator)
View(tipo)
r <- r %>% group_by(r$`Tipo de processo`) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
tipo<-as.data.frame(table(r$`Tipo de processo`))
View(tipo)
View(relator)

table(r$mp)
table(r$Ano_processo)
table(r$Ano_julgado)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3669)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
mean(r$decisao)
r <- r %>% group_by(r$Relator) %>%
  filter(n()>100) %>%
  ungroup() %>%
  droplevels()
relator<-as.data.frame(table(r$Relator))
View(relator)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado+r$Duracao,
            data = r , family = binomial)
summary(glm.fit)
r<-r1
r <- r %>% group_by(r$Relator) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$`Tipo de processo`) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_processo) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_julgado) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
relator<-as.data.frame(table(r$Relator))
tipo<-as.data.frame(table(r$`Tipo de processo`))
table(r$Ano_processo)
table(r$Duracao)
glimpse(r)
r$Ano_processo1 = r$Ano_processo - 1995
table(r$Ano_processo1)
table(r$Ano_julgado)
r$Ano_julgado1=r$Ano_julgado - 2001
table(r$Ano_julgado)
r$Ano_julgado1= r$Ano_julgado - 2001
table(r$Ano_julgado1)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1+r$Duracao,
            data = r , family = binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3669)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
exp(coef(glm.fit))
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
summary(glm.fit)
exp(coef(glm.fit))
summary(glm.fit)
exp(coef(glm.fit))
table(r$Relator)
table(r$`Tipo de processo`)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
coef(glm.fit)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo+r$Ano_julgado,
            data = r , family = binomial)
coef(glm.fit)
exp(coef(glm.fit))
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3669)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
glm.fit=glm(decisao~r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3669)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
r<-r1
r <- r %>% group_by(r$`Tipo de processo`) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_processo) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_julgado) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
glm.fit=glm(decisao~r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3756)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
r <- r %>% group_by(r$Relator) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Relator) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$`Tipo de processo`) %>%
  filter(n()>50) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_processo) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
r <- r %>% group_by(r$Ano_julgado) %>%
  filter(n()>10) %>%
  ungroup() %>%
  droplevels()
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3669)
glm.pred[glm.probs >.5]="1"
mean(glm.pred==r$decisao)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3669)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)
summary(glm.fit)
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
table(r$Ano_processo1)
r$Ano_processo1 = r$Ano_processo - 1995
r$Ano_julgado1= r$Ano_julgado - 2001
glm.fit=glm(decisao~r$Relator+r$`Tipo de processo`+
              r$ministerio+r$fundacao+r$fundo+r$secretaria+r$mp+
              r$Ano_processo1+r$Ano_julgado1,
            data = r , family = binomial)
coef(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.pred=rep("0",3669)
glm.pred[glm.probs >.5]="1"
table(glm.pred,r$decisao)
mean(glm.pred==r$decisao)

r<-r_relator

ggplot(r,aes(x=r$`Tipo de processo`, fill=mp))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("mp", values = c("atuou" = "black", "nao atuou" = "white"))+
  labs(x="tipo de processo", y="frequ?ncia")+theme(legend.position="top")

r1<- r %>% 
  filter(r$`Tipo de processo` != 'APOS')
r1<- r1 %>% 
  filter(r1$`Tipo de processo` != 'PCIV')

ggplot(r1,aes(x=r1$Relator, fill=multa))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("multa", values = c("TRUE" = "black", "FALSE" = "white"))+
  labs(x="relator", y="frequ?ncia")+theme(legend.position="top")

ggplot(r2,aes(x=r2$...FNSMS, fill=multa))+
  geom_bar(position = "dodge",color="black")+
  scale_fill_manual("multa", values = c("TRUE" = "black", "FALSE" = "white"))+
  labs(x="?rg?os ou entidades", y="frequ?ncia")+theme(legend.position="top")

table(r$...31)

table(r2$...FNSMS)
