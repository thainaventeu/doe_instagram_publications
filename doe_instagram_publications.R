install.packages("FrF2", repos = "http://cran.rstudio.com/") # Instala o pacote
library(FrF2) # Carrega o pacote para uso

plan.completo = FrF2(nfactors = 3,
                     nruns = 2^3,
                     replications = 3,
                     randomize = FALSE)

summary(plan.completo)

#personalizar fatores 

                   
                   plan.person <- FrF2(nfactors  = 3,
                                       nruns = 2^3,
                                       replications = 3,
                                       randomize = FALSE,
                                       factor.names = list(
                                         Paisagem = c("ausente", "presente"),
                                         Eu = c("ausente", "presente"),
                                         Cachorro = c("ausente", "presente")))
          
                                    
summary(plan.person) #desenho experimental

resultados = c(37, 48, 153, 100, 50, 48, 159, 107,
               33, 61, 109, 124, 61, 48, 116, 100, 
               61, 86, 150, 128, 60, 44, 146, 107)

plan.atualizado = add.response(design = plan.person, response = resultados)

summary(plan.atualizado)

MEPlot(plan.atualizado) #interação por vetores

#matriz de efeito de interação
IAPlot(plan.atualizado, abbrev = 5, show.alias = TRUE, lwd = 2, cex = 2, cex.xax = 1.2, cex.lab = 1.5, main = "Matriz dos Efeitos de Interação") 

#significancia dos fatores
DanielPlot(plan.atualizado, 
           code = TRUE, 
           half = TRUE, 
           alpha = 0.5)

#gráfico de pareto para visualização de efeitos
install.packages("qualityTools")
library(qualityTools)
fat3_1 <- fracDesign(k = 3, 
                     +                      gen = "E=ABC",
                     +                      centerCube = 0, 
                     +                      replicates = 3)
runOrd(fat3_1) <- standOrd(fat3_1)
fat3_1 <- fracDesign(k = 3, replicates = 3)
runOrd(fat3_1) <- standOrd(fat3_1)
View(fat3_1)
View(fat3_1)
response(fat3_1) <- resultados
paretoPlot(fat3_1,
             +            ylab = "Standadized Effects",
             +            xlab = "Term",
             +            main = "Pareto Chart of the Standadized Effects")

#anova

attach(plan.atualizado)

A <- Paisagem
B <- Eu
C <- Cachorro
anova <- aov(resultados ~ A*B*C)
anova
summary(lm(plan.atualizado))

par(mfrow = c(2,2))
plot(anova)

#https://rpubs.com/brennerbiasi/DoE_PEI
#https://rpubs.com/adelmofilho/post3
#https://rstudio-pubs-static.s3.amazonaws.com/186773_ec69af1e594e412098d93a6612754b54.html#modelo_de_regress%C3%A3o
