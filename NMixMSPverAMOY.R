library(spAbundance)
library(ggplot2)

load("MSPverAMOYNM.rda")
str(MSPverAMOYNM)


abund.formula <- ~ scale(superficie) + scale(year) + (1 | año.s) + (1 | sitio) + (1 | SUID)
abund.formula1 <- ~ scale(superficie) + scale(year)  + (1 | sitio) + (1 | SUID)
abund.formula2 <- ~  scale(year) + (1 | sitio) + (1 | SUID)
abund.formula3 <- ~  scale(superficie) + (1 | año.s) + (1 | sitio) + (1 | SUID)

abund.formula4 <- ~  scale(year) + (1 | sitio) + (1 | SUID)
abund.formula5 <- ~  scale(year) + (1 | sitio) + (1 | SUID)
abund.formula6 <- ~  scale(year)  + sitio + (1 | SUID)
abund.formula7 <- ~  scale(year)  + sitio + (1 | SUID)


det.formula <- ~ precipitacion + viento + (1|año) + (1 | sitio) + (1 | unidad)
det.formula1 <- ~ precipitacion  + (1|año) + (1 | sitio) + (1 | unidad)
det.formula2 <- ~ viento  + (1|año) + (1 | sitio) + (1 | unidad)
det.formula3 <- ~ 1  + (1|año) + (1 | sitio) + (1 | unidad)

det.formula4 <- ~ viento + (1 | sitio) + (1 | unidad)
det.formula5 <- ~ precipitacion  + (1 | sitio) + (1 | unidad)
det.formula6 <- ~ viento + (1 | sitio) + (1 | unidad)
det.formula7 <- ~ precipitacion  + (1 | sitio) + (1 | unidad)


# Pair-wise distances between all sites
dist.mat <- dist(MSPverAMOYNM4.3$coords)
# Exponential covariance model
cov.model <- 'exponential'
# Specify list of inits
inits <- list(alpha = 0,
              beta = 0,
              kappa = 0.5,
              sigma.sq.mu = 0.5,
              N = apply(MSPverAMOYNM4.3$y, 1, max, na.rm = TRUE), 
              sigma.sq = 1, 
              phi = 3 / mean(dist.mat),
              w = rep(0, nrow(MSPverAMOYNM4.3$y)))


priors <- list(alpha.normal = list(mean = 0, var = 2.72),
               beta.normal = list(mean = 0, var = 100), 
               kappa.unif = c(0, 100))


tuning <- list(beta = 0.5, alpha = 0.5, kappa = 0.5, beta.star = 0.5, 
               w = 0.5, phi = 0.5, alpha.star = 0.5)


verbose <- TRUE
batch.length <- 25
n.batch <- 4000
# Total number of MCMC samples per chain
batch.length * n.batch


n.burn <- 30000
n.thin <- 20
n.chains <- 3

out0 <- NMix(abund.formula = abund.formula, 
                 det.formula = det.formula, 
                 data = MSPverAMOYNM, 
                 inits = inits, 
                 priors = priors,
                 n.batch = n.batch,
                 batch.length = batch.length, 
                 tuning = tuning, 
                 n.omp.threads = 1,
                 n.report = 400,
                 family = 'NB',
                 verbose = TRUE,
                 n.burn = n.burn,
                 n.thin = n.thin, 
                 n.chains = n.chains)

summary(out0)
ppc.out.sp0 <- ppcAbund(out0, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp0)

# Abundance regression coefficients
plot(out0, param = 'beta', density = FALSE)
#plot(out0, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out0, param = 'alpha', density = FALSE)
#plot(out0, param = 'alpha.star', density = FALSE)

save(out0, file = 'MSPverAMOYNMout0.rda')


out1 <- NMix(abund.formula = abund.formula1, 
                 det.formula = det.formula1, 
                 data = MSPverAMOYNM, 
                 inits = inits, 
                 priors = priors,
                 n.batch = n.batch,
                 batch.length = batch.length, 
                 tuning = tuning, 
                 n.omp.threads = 1,
                 n.report = 400,
                 family = 'NB',
                 verbose = TRUE,
                 n.burn = n.burn,
                 n.thin = n.thin, 
                 n.chains = n.chains)

summary(out1)
ppc.out.sp1 <- ppcAbund(out1, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp1)

# Abundance regression coefficients
plot(out1, param = 'beta', density = FALSE)
#plot(out1, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out1, param = 'alpha', density = FALSE)
#plot(out1, param = 'alpha.star', density = FALSE)

save(out1, file = 'MSPverAMOYNMout1.rda')

out2 <- NMix(abund.formula = abund.formula2, 
                 det.formula = det.formula2, 
                 data = MSPverAMOYNM, 
                 inits = inits, 
                 priors = priors,
                 n.batch = n.batch,
                 batch.length = batch.length, 
                 tuning = tuning, 
                 n.omp.threads = 1,
                 n.report = 400,
                 family = 'NB',
                 verbose = TRUE,
                 n.burn = n.burn,
                 n.thin = n.thin, 
                 n.chains = n.chains)
summary(out2)
ppc.out.sp2 <- ppcAbund(out2, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp2)

# Abundance regression coefficients
plot(out2, param = 'beta', density = FALSE)
#plot(out2, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out2, param = 'alpha', density = FALSE)
#plot(out2, param = 'alpha.star', density = FALSE)

save(out2, file = 'MSPverAMOYNMout2.rda')

out3<- NMix(abund.formula = abund.formula3, 
                det.formula = det.formula3, 
                data = MSPverAMOYNM, 
                inits = inits, 
                priors = priors,
                n.batch = n.batch,
                batch.length = batch.length, 
                tuning = tuning, 
                n.omp.threads = 1,
                n.report = 400,
                family = 'NB',
                verbose = TRUE,
                n.burn = n.burn,
                n.thin = n.thin, 
                n.chains = n.chains)

summary(out3)
ppc.out.sp3 <- ppcAbund(out3, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp3)

# Abundance regression coefficients
plot(out3, param = 'beta', density = FALSE)
#plot(out3, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out3, param = 'alpha', density = FALSE)
#plot(out3, param = 'alpha.star', density = FALSE)

save(out3, file = 'MSPverAMOYNMout3.rda')

out4 <- NMix(abund.formula = abund.formula4, 
                 det.formula = det.formula4, 
                 data = MSPverAMOYNM, 
                 inits = inits, 
                 priors = priors,
                 n.batch = n.batch,
                 batch.length = batch.length, 
                 tuning = tuning, 
                 n.omp.threads = 1,
                 n.report = 400,
                 family = 'NB',
                 verbose = TRUE,
                 n.burn = n.burn,
                 n.thin = n.thin, 
                 n.chains = n.chains)
summary(out4)
ppc.out.sp4 <- ppcAbund(out4, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp4)

# Abundance regression coefficients
plot(out4, param = 'beta', density = FALSE)
#plot(out4, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out4, param = 'alpha', density = FALSE)
#plot(out4, param = 'alpha.star', density = FALSE)

save(out4, file = 'MSPverAMOYNMout4.rda')

out5 <- NMix(abund.formula = abund.formula5, 
                 det.formula = det.formula5, 
                 data = MSPverAMOYNM, 
                 inits = inits, 
                 priors = priors,
                 n.batch = n.batch,
                 batch.length = batch.length, 
                 tuning = tuning, 
                 n.omp.threads = 1,
                 n.report = 400,
                 family = 'NB',
                 verbose = TRUE,
                 n.burn = n.burn,
                 n.thin = n.thin, 
                 n.chains = n.chains)
summary(out5)
ppc.out.sp5 <- ppcAbund(out5, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp5)

# Abundance regression coefficients
plot(out5, param = 'beta', density = FALSE)
#plot(out5, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out5, param = 'alpha', density = FALSE)
#plot(out5, param = 'alpha.star', density = FALSE)

save(out5, file = 'MSPverAMOYNMout5.rda')

out6 <- NMix(abund.formula = abund.formula6, 
                 det.formula = det.formula6, 
                 data = MSPverAMOYNM, 
                 inits = inits, 
                 priors = priors,
                 n.batch = n.batch,
                 batch.length = batch.length, 
                 tuning = tuning, 
                 n.omp.threads = 1,
                 n.report = 400,
                 family = 'NB',
                 verbose = TRUE,
                 n.burn = n.burn,
                 n.thin = n.thin, 
                 n.chains = n.chains)
summary(out6)
ppc.out.sp6 <- ppcAbund(out6, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp6)

# Abundance regression coefficients
plot(out6, param = 'beta', density = FALSE)
#plot(out6, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out6, param = 'alpha', density = FALSE)
#plot(out6, param = 'alpha.star', density = FALSE)

save(out6, file = 'MSPverAMOYNMout6.rda')

out7 <- NMix(abund.formula = abund.formula7, 
                 det.formula = det.formula7, 
                 data = MSPverAMOYNM, 
                 inits = inits, 
                 priors = priors,
                 n.batch = n.batch,
                 batch.length = batch.length, 
                 tuning = tuning, 
                 n.omp.threads = 1,
                 n.report = 400,
                 family = 'NB',
                 verbose = TRUE,
                 n.burn = n.burn,
                 n.thin = n.thin, 
                 n.chains = n.chains)
summary(out7)
ppc.out.sp7 <- ppcAbund(out7, fit.stat = 'freeman-tukey', group = 1)
summary(ppc.out.sp7)

# Abundance regression coefficients
plot(out7, param = 'beta', density = FALSE)
#plot(out7, param = 'beta.star', density = FALSE)
# Detection regression coefficients
plot(out7, param = 'alpha', density = FALSE)
#plot(out7, param = 'alpha.star', density = FALSE)

save(out7, file = 'MSPverAMOYNMout7.rda')


#------------------WAIC----------------------------

waicAbund(out0)
waicAbund(out1)
waicAbund(out2)
waicAbund(out3)
waicAbund(out4)
waicAbund(out5)
waicAbund(out6)
waicAbund(out7)


