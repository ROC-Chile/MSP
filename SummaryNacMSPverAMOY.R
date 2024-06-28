library(tidyverse)
library(coda)
library(spAbundance)
library(MCMCvis)
library(scales)

load("MSPverAMOY.rda")
load("MSPverAMOYout5.rda")

# Valores de tendencia de la especie
trend.log.samples <- MCMCchains(out5$beta.samples, params = 'scale(year)', exact = TRUE)

n.years <- n_distinct(MSPverAMOY$abund.covs[, 'year'])

# Valores de intercepto
int.samples <- MCMCchains(out5$beta.samples, params = '(Intercept)', exact = TRUE)

# Efectos aleatorios (sitio y unidad anidada)
trend.re.samples <- MCMCchains(out5$beta.star.samples, params = c('sitio', 'SUID'), exact = FALSE)

trend.re.samples <- array(MCMCchains(trend.re.samples, params = 'Intercept', exact = FALSE), 
                          dim = c(nrow(trend.re.samples), n.years))

pred.df <- data.frame(year = 2017:2023)
pred.df$year.s <- (2017:2023 - mean(MSPverAMOY$abund.covs[, 'year'], na.rm = TRUE)) / sd(MSPverAMOY$abund.covs[, 'year'], 
                                                                                                                                                                                  na.rm = TRUE)
n.samples <- nrow(trend.re.samples)

mu.samples <- array(NA, dim = c(n.samples, n.years))
mu.trend.samples <- array(NA, dim = c(n.samples, n.years))

# Calculo de mu y mu.tred
for (j in 1:n.samples) {
  mu.samples[j, ] <- exp(int.samples[j] + 
                           trend.log.samples[j] * pred.df$year.s + 
                           trend.re.samples[j, ])
  mu.trend.samples[j, ] <- exp(int.samples[j] + 
                                 trend.log.samples[j] * pred.df$year.s)
}

# Cuantiles de mu y mu.trend
mu.quants <- apply(mu.samples, 2, quantile, c(0.025, 0.5, 0.975))
mu.trend.quants <- apply(mu.trend.samples, 2, quantile, c(0.025, 0.5, 0.975))

print(mu.quants)
print(mu.trend.quants)

# Transformar a indice 
mu.quants2 <- mu.quants
factor <- 100 / mu.quants[2, 1]  #mediana del primer año para calcular el factor de ajuste
mu.quants2 <- mu.quants * factor

# Ajustar cuantiles de mu.trend en relación a la mediana del primer año 
mu.trend.quants2 <- mu.trend.quants * factor

print(mu.quants2)
print(mu.trend.quants2)

plot.df <- data.frame(med = c(mu.quants2[2, ]),
                      low = c(mu.quants2[1, ]),
                      high = c(mu.quants2[3, ]),
                      med.trend = c(mu.trend.quants2[2, ]),
                      low.trend = c(mu.trend.quants2[1, ]),
                      high.trend = c(mu.trend.quants2[3, ]),
                      year = 2017:2023)

# Muestras posteriores de la pendiente
slope_samples <- MCMCchains(out5$beta.samples, params = 'scale(year)', exact = TRUE)

# Intervalo 2.5-97.5%
slope_mean <- mean(slope_samples)
slope_ci <- quantile(slope_samples, probs = c(0.025, 0.975))

slope_mean
slope_ci

# Grafico
ggplot(data = plot.df, aes(x = year, y = med)) + 
  geom_point(size = 3.5, color = "black") + 
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2, size = 1, color = "black") +
  geom_smooth(method = 'lm', color = "orange", linetype = "dashed", fill = "orange", alpha = 0.2) + 
  theme_bw(base_size = 22) + 
  scale_x_continuous(breaks = 2017:2023) +
  scale_y_continuous(breaks = breaks_pretty(n = 4)) + 
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16)) +
  labs(x = '', y = 'Índice') +
  annotate("text", x = 2019, y = 280, label = "italic('Haematopus palliatus')", size = 6, hjust = 0.5, parse = TRUE) +
  annotate("text", x = 2019, y = 260, label = "Temporada Verano", size = 4, hjust = 0.5)
