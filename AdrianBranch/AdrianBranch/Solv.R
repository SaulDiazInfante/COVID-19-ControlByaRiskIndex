library(plotly)
library(deSolve)

### Functions ###
source("ode1.R")
source("simp38.R")

### Parameters values of functional cost ###
ai <- 3
au <- 1

### Parameters values ###
k <- 200
a <- 0
beta <- 0.3
mu <- 1 / (365 * 70)
phi <- 1 / 500
omega <- 1 / 180
theta <- 1 / 120
sigma <- 0.95
gamma <- 0.1

### Initial conditions ###
V0 <- 0
R0 <- 0
C0 <- 0.45
I0 <- 5
S0 <- 100000 - I0

z1 <- c(S0, I0, V0, R0, C0, 0)

### other parameters ###
uk <- c(0.05, 0.25, 0.5, 0.75, 0.95)
v1 <- 0:52
# VF <- rep(NA, length(v1))
# YS <- matrix(NA, 14, length(v1))
# YI <- matrix(NA, 14, length(v1))
# YV <- matrix(NA, 14, length(v1))
# YR <- matrix(NA, 14, length(v1))
# YC <- matrix(NA, 14, length(v1))
# YF <- matrix(NA, 14, length(v1))

Y3 <- matrix(NA, 14 * length(v1), 8)

### solve system ###

for (j in v1) {
  t0 <- seq(14 * j, 14 * (j + 1), 0.01)
  tfix <- seq(1, length(t0), 100)
  tfix <- tfix[-1]

  temp1 <- 10^10

  for (i in 1:length(uk)) {
    par1 <- c(k, uk[i], a, beta, mu, phi, omega, theta, sigma, gamma)
    Y <- as.matrix(ode(
      func = ode1, y = z1, times = t0, parms = par1[1:10],
      method = "rk4"
    ))[, 1:7]

    Y1 <- Y[tfix, ]
    Func <- ai * Y1[, 3] + au * uk[i]^2
    Val_F <- simp38(Func)

    if (Val_F < temp1) {
      temp1 <- Val_F
      Y2 <- Y1[, 2:7]

      # YS[,(j + 1)] <- Y1[,2]
      # YI[,(j + 1)] <- Y1[,3]
      # YV[,(j + 1)] <- Y1[,4]
      # YR[,(j + 1)] <- Y1[,5]
      # YC[,(j + 1)] <- Y1[,6]
      # YF[,(j + 1)] <- Y1[,7]

      VF <- uk[i]
    }
  }

  z1 <- Y2[nrow(Y2), ]

  t1 <- seq(14 * j, 14 * (j + 1))
  t1 <- t1[-1]

  Y3[t1, 3:8] <- Y2
  Y3[t1, 1] <- t1
  Y3[t1, 2] <- rep(VF, length(t1))
}

### Figures ###
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  title = "Risk",
  side = "right"
)

Fig <- plot_ly()
Fig <- Fig %>% add_trace(
  x = Y3[, 1], y = Y3[, 7], mode = "lines",
  line = list(color = "blue"),
  name = "Prevalence", type = "scatter"
)
Fig <- Fig %>% layout(
  yaxis = list(title = "Number of people"),
  yaxis2 = ay
)
Fig


### Solve system ###
# Controlled system #






# Y11 <- Y1[tfix,]

# Non controlled system #
k3 <- 200
k4 <- 200
par2 <- c(k3, k4, a, beta, mu, phi, omega, theta, sigma, gamma)
Y2 <- as.matrix(ode(
  func = ode1, y = z1, times = t0, parms = par2[1:10],
  method = "rk4"
))[, 1:7]
# Y22 <- Y2[tfix,]

######### Rt #########

Rts1 <- rep("NAN", nrow(Y1))
Rts2 <- rep("NAN", nrow(Y2))
j <- 1

for (t in t0) {
  N1 <- sum(Y1[j, 2:5])

  FOI_1 <- (1 + a * cos(2 * pi * t / 365)) * beta * (Y1[j, 6] / N1)
  Rts1[j] <- FOI_1 * (Y1[j, 2] + (1 - sigma) * Y1[j, 4]) / (mu + gamma)

  FOI_2 <- (1 + a * cos(2 * pi * t / 365)) * beta * (Y2[j, 6] / N1)
  Rts2[j] <- FOI_2 * (Y2[j, 2] + (1 - sigma) * Y2[j, 4]) / (mu + gamma)

  j <- j + 1
}

### Figures ###
ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  title = "Risk",
  side = "right"
)

Fig <- plot_ly()
Fig <- Fig %>% add_trace(
  x = Y1[, 1], y = Y1[, 3], mode = "lines",
  line = list(color = "blue", dash = "dash"),
  name = "Risk Index - C", type = "scatter", yaxis = "y2"
)
Fig <- Fig %>% add_trace(
  x = t0, y = Y2[, 3], mode = "lines",
  line = list(color = "red", dash = "dash"),
  name = "Risk Index - WC",
  type = "scatter", yaxis = "y2"
)
Fig <- Fig %>% add_trace(
  x = t0, y = Rts1, mode = "lines",
  line = list(color = "blue"),
  name = "Eff. RN - C", type = "scatter"
)
Fig <- Fig %>% add_trace(
  x = t0, y = Rts2, mode = "lines",
  line = list(color = "red"),
  name = "Eff. RN - WC", type = "scatter"
)
Fig <- Fig %>% layout(
  yaxis = list(title = "Effective Reproductive Number"),
  yaxis2 = ay,
  title = paste0(
    "I0 - ", I0, " / ", "Low afore - ", k1, " / ",
    "High afore - ", k2, " / ",
    "Afore wo cont - ", k3
  )
)
# Fig2

###

Fig3 <- plot_ly(type = "scatter", mode = "none")
Fig3 <- Fig3 %>% add_trace(
  x = t0, y = Y1[, 7], mode = "lines",
  line = list(color = "blue"),
  showlegend = FALSE, name = "Cum. Incidence - C"
)
Fig3 <- Fig3 %>% add_trace(
  x = t0, y = Y2[, 7], mode = "lines",
  line = list(color = "red"), showlegend = FALSE,
  name = "Cum. Incidence - WC"
)
# Fig3

###
Fig4 <- plot_ly()
Fig4 <- Fig4 %>% add_trace(
  x = t0, y = Y1[, 3], mode = "lines",
  line = list(color = "blue", dash = "dash"),
  name = "Risk Index - C", type = "scatter", yaxis = "y2"
)
Fig4 <- Fig4 %>% add_trace(
  x = t0, y = Y2[, 3], mode = "lines",
  line = list(color = "red", dash = "dash"),
  name = "Risk Index - WC",
  type = "scatter", yaxis = "y2"
)
Fig4 <- Fig4 %>% add_trace(
  x = t0, y = Rts1, mode = "lines",
  line = list(color = "blue"),
  name = "Eff. RN - C", type = "scatter"
)
Fig4 <- Fig4 %>% add_trace(
  x = t0, y = Rts2, mode = "lines",
  line = list(color = "red"),
  name = "Eff. RN - WC", type = "scatter"
)
Fig4 <- Fig4 %>% layout(
  yaxis = list(title = "Effective Reproductive Number"),
  yaxis2 = ay,
  title = paste0(
    "I0 - ", I0, " / ", "Low afore - ", k1, " / ",
    "High afore - ", k2, " / ",
    "Afore wo cont - ", k3
  )
)
###

Fig <- subplot(Fig2, Fig3,
  nrows = 1, titleY = TRUE,
  margin = 0.07
)
Fig <- Fig %>% layout(autosize = F, width = 1500, height = 600)

htmlwidgets::saveWidget(as_widget(Fig), paste0(
  "I0_", I0, "_C0_", C0, "_beta_",
  beta, ".html"
))
