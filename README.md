# OMERF

We propose an innovative statistical method, called Ordinal Mixed-Effect Random Forest (OMERF), that extends the use of random forest to the analysis of hierarchical data and ordinal responses.
The model preserves the flexibility and ability of modeling complex patterns of both categorical and continuous variables, typical of tree-based ensemble methods, and, at the same time, takes into account the structure of hierarchical data, modeling the dependence structure induced by the grouping and allowing statistical inference at all data levels.

A simulation study is conducted to validate the performance of the proposed method and to compare it to the one of other state-of-the art models. The application of OMERF is exemplified in a case study focusing on predicting students performances using data from the Programme for International Student Assessment (PISA) 2022. The model identifies discriminating student characteristics and estimates the school-effect.

\url{http://arxiv.org/abs/2406.03130}

## Methodology

OMERF models the fixed effects through a random forest, combining them to the random effects obtained using a CLMM, in order to take into account both possible complex functional forms in the fixed effect component and the nested structure of data.
The model can be formulated as follow:
\begin{equation}
    \label{eq:omerf}
    \begin{aligned}
        \eta_{ijc}  = g(\gamma_{ijc}) &= \theta_{c} - (f(\bm{x}{ij}) + \bm{z}{ij}^T \bm{b}_{i})\\
        g(\gamma_{ijc}) = logi&t(\gamma_{ijc}) = \ln(\frac{\gamma_{ijc}}{\gamma_{ijc}-1}) \\
        \gamma_{ijc} &= \mathbb{P}(y_{ij} \leq c) \\
        \bm{b}{i} &\sim \mathcal{N}_Q(\bm{0},\bm{\Sigma{b}}) \\
        j=1,\dots,n_{i} \qquad i&=1,\dots,I \qquad c=1,\dots,C-1
    \end{aligned}
\end{equation}
where $f(\bm{x}_{ij})$ is the unknown and nonlinear structure estimated through the random forest, $\gamma_{ijc}$ are cumulative probabilities, $\pi_{ijc} = \mathbb{P}(y_{ij}=c) = \mathbb{P}(y_{ij} \leq c) - \mathbb{P}(y_{ij} \leq c-1) = logit^{-1}(\theta_{c} - (f(\bm{x}{ij}) + \bm{z}{ij}^T \bm{b}{i})) - logit^{-1}(\theta{c-1} - (f(\bm{x}{ij}) + \bm{z}{ij}^T \bm{b}{i}))$ is the probability that the $j$-th observation, within the $i$-th group, falls in the $c$-th category. %{and $\eta{ijc}$ is the linear predictor}.
Similarly to CLMM model, OMERF model assumes that the random effects $\bm{b}i$ and $\bm{b}{i'}$ are independent for $i \neq i'$.



