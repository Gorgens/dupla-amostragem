# Importar planilha eletronica
egrandis <- read.csv("egrandis.csv")
egrandis <- read.csv("egrandis.csv", header= TRUE, sep=",", dec=".")

  # Conferir arquivos importados
  dim(egrandis)
  names(egrandis)
  head(egrandis)
  summary(egrandis)

# Resumo geral do data.frame
by(egrandis,egrandis$regiao,summary)

# Calcular a biomassa em nivel de arvore
# Considerar a equacao: Y = 1.22*DBH^2*HT*0.01 em kg de massa seca por árvore
biomass = 1.22 * egrandis$dap^2 * egrandis$ht * 0.01
egrandis = cbind(egrandis, biomass)
rm(biomass)

  # Conferir novo data.frame
  dim(egrandis)
  summary(egrandis)
  by(egrandis, egrandis$faz, summary)

# Criar identificador unico para cada parcela
id = paste(egrandis$regiao, "_", egrandis$faz, "_", egrandis$proj, "_", egrandis$parcela, sep="") 
egrandis = cbind(id, egrandis)
rm(id)

  # Conferir novo data.frame
  dim(egrandis)
  names(egrandis)
  dim(table(egrandis$id))

# Calcular o numero de parcelas em cada estrato (estrato = regiao)
j = 1
amostras = data.frame()
for (i in levels(egrandis$regiao)){
  t = subset(egrandis, egrandis$regiao == i)
  a = table(t$id)
  amostras[j, 1] = i 
  amostras[j, 2] = (dim(a[a>0]))
  j = j + 1
}
rm(i, t, a, j)
names(amostras) = c("regiao", "n")

# Número total da amostras da populacao
# estrato Bofete = 843 hectares
# estrato Botucatu = 250 hectares
# estrato Itatinga = 213 hectares

N = c(843*10000/400, 250*10000/400, 213*10000/400)
amostras = cbind(amostras, N)
rm(N)

# Totalizando a parcela
mean_plot = aggregate(egrandis[ , c("dap","ht")], list(local=egrandis$id), mean)
sd_plot = aggregate(egrandis[ , c("dap","ht")], list(local=egrandis$id), sd)
biomass_plot = aggregate(egrandis$biomass, list(local=egrandis$id), sum)
names(biomass_plot)[2] = "biomassa"
t = na.omit(egrandis)
hd_plot = aggregate( t$hdom, list(local=t$id), mean)
names(hd_plot)[2] = "hdom"
rm(t)
   
   # Alternativa usando tapply
   dap.mean = tapply(egrandis$dap, egrandis$id, mean)
   ht.mean = tapply(egrandis$ht, egrandis$id, mean)
   parc = cbind(dap.mean, ht.mean)
   parc = as.data.frame(parc)
   rm(dap.mean, ht.mean)

parc = merge(mean_plot, sd_plot, by="local", suffixes=c(".mean",".sd"))
parc = merge(parc, hd_plot, by="local", all.x=TRUE)
parc = merge(parc, biomass_plot, by="local", all.x=TRUE)
rm(mean_plot, sd_plot, biomass_plot, hd_plot)

# Calcular biomassa por hectare assumindo parcela de 400 m2 - kg massa seca/ha
biomassa_ha = parc$biomassa * (10000/400)
parc = cbind(parc, biomassa_ha)
rm(biomassa_ha)

  #Verificar novo data.frame
  dim(parc)
  head(parc)


# Recuperar o estrato de cada parcela
j = 1
regiao = 0
for (i in parc$local){
  regiao[j] = unlist(strsplit(as.character(i), split="\\_"))[1]
  j = j + 1
}
rm(i, j)
parc = cbind(regiao, parc)
rm(regiao)

# Media por estrato
mean_regiao = aggregate(parc$biomassa_ha, list(regiao=parc$regiao), mean)
colnames(mean_regiao) = c("regiao", "mean.biomassa_ha")
var_regiao = aggregate(parc$biomassa_ha, list(regiao=parc$regiao), var)
colnames(var_regiao) = c("regiao", "var.biomassa_ha")
estrato = merge(mean_regiao, var_regiao, by="regiao", all.x=TRUE)
estrato = merge(estrato, amostras, by="regiao", all.x=TRUE)
rm(mean_regiao, var_regiao, amostras)

# Media estratificada para a população kg.dm/ha
media.estrat = sum(estrato$mean.biomassa_ha * estrato$N /sum(estrato$N))

# Biomassa do projeto em kg.dm
biomassa.total = media.estrat * sum(estrato$N)

# Biomassa de cada estrato
biomassa.estrato = estrato$mean.biomassa_ha * estrato$N

# Desvio-padrao de cada estrato
desvPad.estrato = sqrt(estrato$var.biomassa_ha)

# Variância da média estratificada
var.estrat = sum(desvPad.estrato * estrato$N/sum(estrato$N))^2/sum(estrato$n)

# Erro padrão da estimativa
Erro.padrao = sqrt(var.estrat)

# Erro percentual
Erro.Percentual = Erro.padrao * qt(0.975, sum(estrato$n)) / media.estrat * 100

  # Transformar kg.dm/ha em tonC/ha e em tonCO2/ha
  media.carbono = media.estrat / 1000 * 0.47
  media.co2 = media.carbono * 3.67
