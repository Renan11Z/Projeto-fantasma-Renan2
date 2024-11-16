library(readxl)
library(tidyverse)
library(rcompanion)
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5)
      ,
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}
#########################################
print_quadro_resumo <- function(data, var_name, title="Medidas resumo
da(o) [nome da variável]", label="quad:quadro_resumo1") {
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name),na.rm = T),2),
              `Desvio Padrão` = round(sd(!!sym(var_name),na.rm = T),2),
              `Variância` = round(var(!!sym(var_name),na.rm = T),2),
              `Mínimo` = round(min(!!sym(var_name),na.rm = T),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs =
                                              .25,na.rm = TRUE),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5,na.rm = TRUE)
                                ,2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs =
                                              .75,na.rm = TRUE),2),
              `Máximo` = round(max(!!sym(var_name),na.rm = T),2)) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column()
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n
", sep="")
  }
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor}
\\\\\n", sep="")
  }
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse =
                                                " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
  \t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  writeLines(latex)
}

####################################################
estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091",
  "#041835", "#666666" )
#tratamento do banco##################################
P1<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=1, col_names=TRUE)
colnames(P1)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P2<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=2, col_names=TRUE)
colnames(P2)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P3<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=3, col_names=TRUE)
colnames(P3)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P4<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=4, col_names=TRUE)
colnames(P4)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P5<-read_excel("C:/Users/marqu/Documents/Estat/Olimpiadas 2000 - 2016.xlsx", sheet=5, col_names=TRUE)
colnames(P5)<-c("Nome","Sexo","Idade","Altura(m)","Peso(kg)","País","Esporte","Modalidade","Medalha")
P<-rbind(P1,P2,P3,P4,P5)
P<-P[!(is.na(P[,9])),]
P$`Altura(m)`<-P$`Altura(m)`/100
P$`Peso(kg)`<-P$`Peso(kg)`/2.205
#Analise 1########################################################
PF<-P[P[,2]=="F",]
PF2<-PF
PF<-PF[!duplicated(PF$Nome),]
PF2<-PF2[!duplicated(PF2$Nome),]
sd(classes$n)
length(classes$n)
Psd<-PF2  %>% count(País,Medalha)
Psd<-Psd[!(is.na(Psd[,2])),]
sum(Psd$n)
classes <- PF %>%
  filter(!is.na(País)) %>%
  count(País) %>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
c<-classes %>% filter(relative_freq>5)
c2<-classes %>% filter(n>5)
c[c(c[,1]=="United States"),1]<-"Estados Unidos"
c[c(c[,1]=="Germany"),1]<-"Alemanha"
ggplot(c)+aes(x = fct_reorder(País, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7)+geom_text(position = position_dodge(width = .9),vjust = -0.5,size = 3)+theme_estat()+labs(x="Países",y="Frequência")
#####################################################################################
length(unique(P$Nome))
Pd<-P[!duplicated(P$Nome),]
length(Pd$Sexo=="F")
###########Análise 2##############################
PMD<-P[!duplicated(P$Nome),]
#PMD$IMC<-PMD$`Peso(kg)`/(PMD$`Altura(m)`)^2
PA2 <- filter(P,Esporte %in% c("Gymnastics",
                                                         "Football","Judo",
                                                         "Athletics",
                                                         "Badminton"))
PA2<-PA2[!duplicated(PA2$Nome),]
####Não funciona#####PA2<-PMD[PMD[,7]==c("Badminton","Judo","Gymnastics","Athletics","Football"),]
PA2[c(PA2[,7]=="Athletics"),7]<-"Atletismo"
PA2[c(PA2[,7]=="Gymnastics"),7]<-"Ginastica"
PA2[c(PA2[,7]=="Football"),7]<-"Futebol"
PA2$IMC<-PA2$`Peso(kg)`/(PA2$`Altura(m)`)^2
ggplot(PA2, aes(x = fct_reorder(Esporte, IMC, median),y=IMC))+geom_boxplot(fill = c("#A11D21"), width = 0.5)+theme_estat()+stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+xlab("Esporte")
round(mean(PA2[PA2[,7]=="Judo",]$IMC,na.rm = T),digits = 2)
PA21%>%group_by(Esporte)%>%print_quadro_resumo(var_name = IMC)
modelo<-lm(PA2$IMC~PA2$Esporte)                 
summary(modelo)
round(sd(PA2[PA2[,7]=="Judo",]$IMC,na.rm = T),digits = 2)
a<-round(mean(PA2[PA2[,7]=="Atletismo",]$IMC,na.rm = T),digits = 2)
GM<-round(mean(PA2[PA2[,7]=="Ginastica",]$IMC,na.rm = T),digits = 2)
round(mean(PA2[PA2[,7]=="Badminton",]$IMC,na.rm = T),digits = 2)
b<-round(median(PA2[PA2[,7]=="Atletismo",]$IMC,na.rm = T),digits = 2)
d<-round(sd(PA2[PA2[,7]=="Atletismo",]$IMC,na.rm = T),digits = 2)
(a-b)/d
round(mean(PA2[PA2[,7]=="Futebol",]$IMC,na.rm = T),digits=2)
Bsd<-round(sd(PA2[PA2[,7]=="Badminton",]$IMC,na.rm = T),digits = 2)
GMe<-round(median(PA2[PA2[,7]=="Ginastica",]$IMC,na.rm = T),digits = 2)
Gsd<-round(sd(PA2[PA2[,7]=="Ginastica",]$IMC,na.rm = T),digits = 2)
(GM-GMe)/Gsd
mean(PA2$IMC,na.rm = T)
#####################Analise 3###############################
#descobrindo os maiores medalhistas
PT3<-P
PT3<-mutate(PT3,QM=is.character(P$Medalha))
PT3$QM<-as.numeric(PT3$QM)
PT3<-PT3%>%group_by(Nome)%>%summarise(Total=sum(QM))
#Michael Fred Phelps, II   Natalie Anne Coughlin (-Hall)    Ryan Steven Lochte
PA3.1<-P[P[,1]=="Michael Fred Phelps, II",]
PA3.2<-P[P[,1]=="Natalie Anne Coughlin (-Hall)",]
PA3.3<-P[P[,1]=="Ryan Steven Lochte",]
PA3<-rbind(PA3.1,PA3.2,PA3.3)
PA3[c(PA3[,9]=="Gold"),9]<-"Ouro"
PA3[c(PA3[,9]=="Silver"),9]<-"Prata"
GA3 <- PA3 %>%
  mutate(Nome = case_when(
    Nome %>% str_detect("Michael Fred Phelps, II") ~ "Michael Fred Phelps, II",
    Nome %>% str_detect("Ryan Steven Lochte") ~ "Ryan Steven Lochte",
    Nome %>% str_detect("Natalie Anne Coughlin (-Hall)") ~ "Natalie Anne Coughlin (-Hall)"
  )) %>%
  group_by(Nome, Medalha ) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )
GA3[c(is.na(GA3[,1])==TRUE),1]<-"Natalie Anne Coughlin (-Hall)"
porcentagens <- str_c(GA3$freq_relativa, "%") %>% str_replace("
\\.", ",")
legendas <- str_squish(str_c(GA3$freq, " (", porcentagens, ")")
)
ordem<-c("Bronze","Prata","Ouro")
GA3$Medalha<-fct_relevel(c(GA3$Medalha),ordem)
ggplot(GA3) +
  aes(
    x = fct_reorder(Nome, freq, .desc = T), y = freq,
    fill = Medalha, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Atleta", y = "Frequência") +
  theme_estat()
#############################
GA3$Medalha<-fct_relevel(c(GA3$Medalha),ordem)
ggplot(GA3) +
  aes(
    x = fct_reorder(Nome, Nome, .desc = F), y = freq,
    fill = Medalha, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Atleta", y = "Frequência") +
  theme_estat()+scale_y_continuous(limits= c(0,25))
#############################
tabela<-xtabs(~PA3$Nome+PA3$Medalha)
cramerV(tabela)
#O coeficiente V de cramer assumiu o valor de 0.3505 demonstrando uma associação fraca-moderada.
#############Analise 4#######################
ggplot(PMD)+geom_point(aes(x=`Peso(kg)`,y=`Altura(m)`),colour = "#A11D21", size = 2,alpha=0.5)+theme_estat()
cor(PMD$`Altura(m)`,PMD$`Peso(kg)` ,use= "complete.obs",method = "pearson")
cor(PMD$`Altura(m)`,PMD$`Peso(kg)` ,use= "complete.obs",method = "spearman")

coefV<-function(x) {
  y<-sd(x,na.rm = T)/mean(x,na.rm = T)
  return(y)
}
coefV(PMD$`Altura(m)`)
coefV(PMD$`Peso(kg)`)
PMD%>%print_quadro_resumo(var_name = `Peso(kg)`)
PMD%>%print_quadro_resumo(var_name = `Altura(m)`)
