---
output: 
  html_document: 
    toc: yes
    fig_caption: yes
    number_sections: yes
    keep_md: yes
    df_print: tibble
---

# Model forumulation

Our formulation without control reads

```{=tex}
\begin{equation}
    \begin{aligned}
        S^{\prime} (t)&= 
            \mu N - \lambda_f S - (\phi + \mu) S + \omega V + \theta R
        \\
        I ^{\prime} (t) &= 
            \lambda_f S + (1 - \sigma) \lambda_f V  - (\gamma + \mu ) I
        \\
        V ^{\prime} (t) &= 
            \phi S - (1 - \sigma) \lambda_f V  - (\omega + \mu) V
        \\
        R ^{\prime} (t) &= 
            \gamma I - (\theta + \mu) R
        \\
        \frac{dC}{dt} &=
            \frac{\kappa}{N-I}
            (1 - C) I^{\prime}(t)
    \end{aligned}

  (\#eq:uncontrolledModel)

\end{equation}
```

With force infection

```{=tex}
\begin{equation}
  \lambda_f := 
    \left[
      \left(
        1 + a 
        \cos 
          \left(
            \frac{2 \pi}{365} t
          \right) 
        \beta_0
      \right)
    \right] 
    \frac{I (1 - C)}{N}.
  
  (\#eq:forceInfection)

\end{equation}
```

To describe epidemic protocols about number the size of a gathering--- the
number of individuals in a meeting---we describe policies assuming that a
decision maker choose an strategy each week from a finite and countable set of
actions. Thus letting a partition of one according to the risk we define the
closed loop policy

```{=tex }
\begin{equation} 
    u_{\beta} = u_{\beta} (C) := 
    \begin{cases}
      u_{\beta} ^ 1, & \text{if } \pi_0 \leq C \leq \pi_i
        \\
        \vdots & \vdots
        \\
        u_{\beta} ^ i, & \text{if } \pi_{i-1} \leq C \leq \pi_i
        \\
        \vdots & \vdots
        \\
        u_{\beta} ^ n, & \text{if } C \geq \pi_n
    \end{cases}
    
    (\#eq:betaControl)
    
\end{equation}
```

where

```{=tex }
\begin{equation}
        u_{\beta} ^ i \in [0, 1], 
        \qquad
        \sum_{i=1}^ n  u_{\beta} ^ {i} =1
\end{equation}
```

For example, Mexico has been aligned with strategies following the analogy of a
traffic light. That is, according to the number of confirmed reported cases and
other calculations regarding with the instant reproductive number $R_t$, the
level risk is classified as green, yellow, orange or red. Thus, according to
this stratification, the health consult recommends a certain set of protocols.

Hence, in the following we consider the control defined on \@ref(eq:betaControl)
as

```{=tex }
\begin{equation} 
    \begin{aligned}
      u_{\beta} (C) &:= 
          u_{\beta} ^ i, \qquad \text{if } \pi_{i-1} \leq C \leq \pi_i
          \\
          i & \in \{ \text{green, yellow, ambar, orange, red}\}
    \end{aligned}
    
    (\#eq:betaControlTrafictLight)
    
\end{equation}
```



