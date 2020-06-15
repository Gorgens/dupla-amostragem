#-----------------------------------------------------------------------------------
# Comparaçãoo entre:
#   Amostragem Casual Simples
#   Amostragem Casual Estratificada
#   Dupla Amostragem por regressao
# 
# Usa métodos e fórmulas apresentados por
#         Shiver & Borders (1996) Sampling techniques for Forest Resource Inventory
#         As fórmulas aparecem indicadas nos comentários entre colchetes     F[_._]
# ESALQ/USP:      Artigo para a Scientia Forestalis
# Código R:       Eric Gorgens, Danitiele Laranja, Luiz C. E. Rodriguez
# Última Revisão: 16/Julho/2015
#-----------------------------------------------------------------------------------
# DATA FRAMEs para armazernar:
#   dados das parcelas amostrais (amostra pequena que inclui valores de P90)
#   estratos do cen?rio A (por material gen?tico: SP0007, SP0581, SP0617, SP0791)
#   estratos do cen?rio B (por matgen e UP: 52I212/17/20, 52I212, 52I216, 52I219)
#   dados para ocen?rio C (c/ valores de P90 da vari?vel auxiliar na amostra grande)
#-----------------------------------------------------------------------------------
fase1_gde <- read.csv('fase1.csv', header=TRUE, sep=";", dec=".")
fase2_pqn <- read.csv('fase2.csv', header=TRUE, sep=",", dec=".")
##estratos2 <- read.csv('estratos_porUPeClone.csv',   header=TRUE, sep=";", dec=".")
#-----------------------------------------------------------------------------------
# Armazena dados "Volume Total Com Casca" e "Percent?l 90" coletados nas amostras 
#   da fase 2 nos vetores y e x, e "Percent?l 90" coletado nas amostras da fase 1
#-----------------------------------------------------------------------------------
x1g <- fase1_gde$ElevP90
x2g <- fase1_gde$ARMFR
x1p <- fase2_pqn$P90
x2p <- fase2_pqn$ARMFR
y   <- fase2_pqn$VTCC
#-----------------------------------------------------------------------------------
# DATA FRAME para armazenamento das estatísticas de comparação
#            entre os métodos de amostragem
#-----------------------------------------------------------------------------------
comparativo <- data.frame(Metodo       = NA, # 1. método de amostragem
                          Estimativa   = NA, # 2. valor do parâmetro estimado
                          MargemIC     = NA, # 3. "bound" do intervalo de confiança
                          Variancia    = NA, # 4. variância da estimativa
                          CV           = NA, # 5. coeficiente de variação
                          ErroPadrao   = NA, # 6. Erro padrão
                          ErroAmostral = NA) # 7. erro amostral % da estimativa
#===================================================================================
# Amostragem casual simples (ACS)
#   N = tamanho da população = [Área Total]/[Área da Parcela]
#   n = número total de amostras = 37 observações de volume (VTCC)
#===================================================================================
comparativo[1,1] <- "ACS"                            # armazena método no DATA FRAME
#-----------------------------------------------------------------------------------
# Cálculo das principais estatísticas da amostragem casual simples (ACS)
#-----------------------------------------------------------------------------------
N      <- fase2_pqn$N[1]           # tamanho da pop.
n      <- length(fase2_pqn$VTCC)   # número de amostras medidas
#***********************************************************************************
yA     <- mean(y)              # (média da amostra de 37 parcelas)      Estimativa A 
#***********************************************************************************
yvaA   <- var(y)               # variância de y na amostra
ydpA   <- sd(y)                # desvio padrão (DP) na amostra
yAvar  <- (yvaA/n)*((N-n)/N)   # variância da estimativa                      F[3.2]
yAdp   <- sqrt(yAvar)          # DP da estimativa        (erro padrão)
yAcv   <- 100*yAdp/yA          # coeficiente de variação da estimativa
tTab   <- qt(.95, n - 1)       # t tabelado COM n-1 graus de liberdade
yAic   <- tTab*yAdp            # "bound" do IC sem fator de correção          F[3.5]
yAea   <- yAic/yA*100          # ERRO AMOSTRAL % sem fator de correção F[AE, pag 46]
#-----------------------------------------------------------------------------------
# armazena resultados no DATA FRAME
#-----------------------------------------------------------------------------------
comparativo[1,2] = yA          # Estimativa A
comparativo[1,3] = yAic        # Intervalo de confiança
comparativo[1,4] = yAvar       # Variância da estimativa
comparativo[1,5] = yAcv        # Coeficiente de variação da estimativa
comparativo[1,6] = yAdp        # Erro padrão
comparativo[1,7] = yAea        # Erro amostral %

#==================================================================================
# Amostragem casual estratificada (ACE por clone)
# N  = Número total de parcelas = [Área Total]/[Área da Parcela]
# n  = Parcelas amostradas      = 37 observações de volume (VTCC)
# L  = Número de estratos       = 4 (quatro clones)
# Nh = Número de amostras possíveis no estrato h
# nh = número de observações na amostra do estrato h
#===================================================================================
comparativo[2,1] <- "ACE Clone"                      # armazena método no DATA FRAME
#-----------------------------------------------------------------------------------
# Cálculo das principais estatísticas da amostragem casual estratificada
#-----------------------------------------------------------------------------------
Nh    <- tapply(fase2_pqn$NblocoA, fase2_pqn$BlocoA, mean)   # tamanho da pop./estrato
nh    <- tapply(fase2_pqn$NblocoA, fase2_pqn$BlocoA, length) # amostras medidas por estrato
yh    <- tapply(fase2_pqn$VTCC, fase2_pqn$BlocoA, mean)      # y médio por estrato
yvah  <- tapply(fase2_pqn$VTCC, fase2_pqn$BlocoA, var)       # variância por estrato
ydph  <- tapply(fase2_pqn$VTCC, fase2_pqn$BlocoA, sd)        # desvio padrão por estrato
yBvah <- (yvah/nh)*((Nh-nh)/Nh)                              # variâncias das estimativas por estrato
yBdph <- sqrt(yBvah)                                         # DP das estimativas por estrato
yBcvh <- 100*yBdph/yh                                        # CVs estimativas por estrato
#***********************************************************************************
yB    <- sum(Nh*yh)/N                                        # Estimativa B
#***********************************************************************************
yBvar <- sum((Nh/N)^2*yBvah)   # variância da estimativa                      F[5.7]
yBdp  <- sqrt(yBvar)           # DP da estimativa (erro padrão)
yBcv  <- 100*yBdp/yB           # CV estimativa
yBic  <- tTab*yBdp             # "bound" do IC sem fator de correção          F[3.5]
yBea  <- yBic/yB*100           # ERRO AMOSTRAL % sem fator de correção F[AE, pag 46]
#-----------------------------------------------------------------------------------
# armazena resultados da ACE clone no DATA FRAME
#-----------------------------------------------------------------------------------
comparativo[2,2] = yB          # Estimativa B
comparativo[2,3] = yBic        # Intervalo de confiança
comparativo[2,4] = yBvar       # Variância da estimativa
comparativo[2,5] = yBcv        # Coeficiente de variação da estimativa
comparativo[2,6] = yBdp        # Erro padrão
comparativo[2,7] = yBea        # Erro amostral

#==================================================================================
# Amostragem casual estratificada (ACE por clone e UP)
# N  = Número total de parcelas = [Área Total]/[Área da Parcela]
# n  = Parcelas amostradas      = 37 observações de volume (VTCC)
# L  = Número de estratos       = 6 (seis grupos UPxClone)
# Nh = Número de amostras possíveis no estrato h
# nh = número de observações na amostra do estrato h
#==================================================================================
comparativo[3,1] <- "ACE Clone e UP"                # armazena método no DATA FRAME
#-----------------------------------------------------------------------------------
# Cálculo das principais estatísticas da amostragem casual estratificada Clone e UP
#-----------------------------------------------------------------------------------
Nh    <- tapply(fase2_pqn$NblocoB, fase2_pqn$BlocoB, mean)     # tamanho da pop./estrato
nh    <- tapply(fase2_pqn$NblocoB, fase2_pqn$BlocoB, length)   # amostras medidas por estrato
yh    <- tapply(fase2_pqn$VTCC, fase2_pqn$BlocoB, mean)        # y médio por estrato
yvah  <- tapply(fase2_pqn$VTCC, fase2_pqn$BlocoB, var)         # variância por estrato
ydph  <- tapply(fase2_pqn$VTCC, fase2_pqn$BlocoB, sd)          # desvio padrão por estrato
yCvah <- (yvah/nh)*((Nh-nh)/Nh)                                # variâncias das estimativas por estrato
yCdph <- sqrt(yCvah)                                           # DP das estimativas por estrato
yCcvh <- 100*yCdph/yh                                          # CVs estimativas por estrato
#***********************************************************************************
yC    <- sum(Nh*yh)/N                                                 # Estimativa C
#***********************************************************************************
yCvar <- sum((Nh/N)^2*yCvah)   # variância da estimativa                      F[5.7]
yCdp  <- sqrt(yCvar)           # DP da estimativa (erro padrão)
yCcv  <- 100*yCdp/yC           # CV estimativa
yCic  <- tTab*yCdp             # "bound" do IC sem fator de correção          F[3.5]
yCea  <- yCic/yC*100           # ERRO AMOSTRAL % sem fator de correção F[AE, p?g 46]
#-----------------------------------------------------------------------------------
# armazena resultados da ACE Clone e UP clone no DATA FRAME
#-----------------------------------------------------------------------------------
comparativo[3,2] = yC          # Estimativa B
comparativo[3,3] = yCic        # Intervalo de confiança
comparativo[3,4] = yCvar       # Variância da estimativa
comparativo[3,5] = yCcv        # Coeficiente de variação da estimativa
comparativo[3,6] = yCdp        # Erro padrão
comparativo[3,7] = yCea        # Erro amostral

#===================================================================================
# Dupla Amostragem com Regressão Linear VTCC = f(P90) baseada nas fórmulas 
# de Shiver & Borders
#   Fase 1 (amostra grande)
#     Grid com 10.764 células (de área igual as das parcelas da fase 2) para as
#     quais foi calculado o percentil 90 (x1g=ElevP90)
#   Fase 2 (amostra pequena)
#     Conjunto de 37 parcelas amostrais com área = 0.038013 ha nas quais se mediu
#     o volume (y=VTCC) e o percentil 90 (x=P90)
#===================================================================================
comparativo[4,1] <- "DA com P90"                    # armazena método no DATA FRAME
#-----------------------------------------------------------------------------------
# Cálculo das principais estatísticas da amostragem em duas fases (dupla amostragem)
#-----------------------------------------------------------------------------------
ng    <- fase2_pqn$N[1]            # número de observações na amostra grande
n      <- length(fase2_pqn$VTCC)   # número de amostras medidas
#-----------------------------------------------------------------------------------
# confere resultados da regressão com a função lm()
#-----------------------------------------------------------------------------------
Y_fx1p = lm(y ~ x1p)                       # Regressão Linear Simples y=f(x1)
summary(Y_fx1p)                            # Resumo da regressão
anova(Y_fx1p)                              # Quadro de análise de variância
# par(mfrow=c(2,2))                        # Parametriza layout gráfico
# plot(Y_fx1p)                             # Análise gráfica de resíduos
rhoD = sqrt(summary(Y_fx1p)$r.squared)     # F[7.5]

#***********************************************************************************
yD     <- predict(Y_fx1p, data.frame(x1p = mean(x1g)))                   # Estimativa D
#***********************************************************************************
# Fórmula apresentada em DEVRIES (1986) para a variância da estimativa yD     F[7.4]
#-----------------------------------------------------------------------------------
yDvar <- (yvaA/n)*(1-(((ng-n)/ng)*(rhoD^2)))
#-----------------------------------------------------------------------------------
yDdp  <- sqrt(yDvar)          # DP da estimativa (erro padrão)
yDcv  <- 100*yDdp/yD          # CV estimativa
yDic  <- tTab*yDdp            # "bound" do IC sem fator de correção          F[3.5]
yDea  <- yDic/yD*100          # ERRO AMOSTRAL % sem fator de correção F[AE, pág 46]
#-----------------------------------------------------------------------------------
# armazena resultados da DA com P90 no DATA FRAME
#-----------------------------------------------------------------------------------
comparativo[4,2] = yD          # Estimativa D
comparativo[4,3] = yDic        # Intervalo de confian?a
comparativo[4,4] = yDvar       # Vari?ncia da estimativa
comparativo[4,5] = yDcv        # Coeficiente de varia??o da estimativa
comparativo[4,6] = yDdp        # Erro padr?o
comparativo[4,7] = yDea        # Erro amostral