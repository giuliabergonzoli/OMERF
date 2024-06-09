# OMERF

We propose an innovative statistical method, called Ordinal Mixed-Effect Random Forest (OMERF), that extends the use of random forest to the analysis of hierarchical data and ordinal responses.
The model preserves the flexibility and ability of modeling complex patterns of both categorical and continuous variables, typical of tree-based ensemble methods, and, at the same time, takes into account the structure of hierarchical data, modeling the dependence structure induced by the grouping and allowing statistical inference at all data levels.

This repository contains the following directories:
- ```sim_study```: R implementation of the simulation study conducted to validate the performance of the proposed method and to compare it to the one of other state-of-the art models;
- ```case_study```: R implementation of the case study focusing on predicting students performances using data from the Programme for International Student Assessment (PISA) 2022;
- ```data```: R implementation of the OMERF algorithm and of the indices employed as performance measures; R scripts for the simulation study datasets' design; processed PISA 2022 data used for the case study.

The arXive of the paper (arXiv:2406.03130 [stat.ME]) is available at http://arxiv.org/abs/2406.03130

## Methodology

OMERF models the fixed effects through a random forest, combining them to the random effects obtained using a CLMM. The model can be formulated as follow:
```math
    \begin{aligned}
        \eta_{ijc}  = g(\gamma_{ijc}) &= \theta_{c} - (f(\mathbf{x}_{ij}) + \mathbf{z}_{ij}^T \mathbf{b}_{i})\\
        g(\gamma_{ijc}) = logi&t(\gamma_{ijc}) = \ln(\frac{\gamma_{ijc}}{\gamma_{ijc}-1}) \\
        \gamma_{ijc} &= \mathbb{P}(y_{ij} \leq c) \\
        \mathbf{b}{i} &\sim \mathcal{N}_Q(\mathbf{0},\mathbf{\Sigma_{b}}) \\
        j=1,\dots,n_{i} \qquad i&=1,\dots,I \qquad c=1,\dots,C-1
    \end{aligned}
```

where $f(\boldsymbol{x}_ {ij})$ is the unknown and nonlinear structure estimated through the random forest, $\gamma_{ijc}$ are cumulative probabilities, $\pi_{ijc} = \mathbb{P}(y_{ij}=c) = \mathbb{P}(y_{ij} \leq c) - \mathbb{P}(y_{ij} \leq c-1) = logit^{-1}(\theta_{c} - (f(\mathbf{x}_ {ij}) + \mathbf{z}_ {ij}^T \mathbf{b}_ {i})) - logit^{-1}(\theta_ {c-1} - (f(\mathbf{x}_ {ij}) + \mathbf{z}_ {ij}^T \mathbf{b}_ {i}))$ is the probability that the $j$-th observation, within the $i$-th group, falls in the $c$-th category.

Similarly to CLMM model, OMERF model assumes that the random effects $\mathbf{b}_ i$ and $\mathbf{b}_ {i'}$ are independent for $i \neq i'$.



