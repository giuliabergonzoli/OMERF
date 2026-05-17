library(ordinal)
library(ranger)
library(ordinalForest)
library(pdfCluster)
library(psych)

source('OMERF_ranger.R')
source('OMERF_clm_ranger.R')
source('build_dataset_no_ranef_4categories.R')
source('ord_class_index.R')
source('index.R')

# -------------------------------------------------------
# Helper functions
# -------------------------------------------------------

macro_precision <- function(y_true, y_pred, lev) {
  prec <- sapply(lev, function(c) {
    tp <- sum(y_true == c & y_pred == c)
    fp <- sum(y_true != c & y_pred == c)
    if ((tp + fp) == 0) return(NA) else return(tp / (tp + fp))
  })
  mean(prec, na.rm = TRUE)
}

macro_recall <- function(y_true, y_pred, lev) {
  rec <- sapply(lev, function(c) {
    tp <- sum(y_true == c & y_pred == c)
    fn <- sum(y_true == c & y_pred != c)
    if ((tp + fn) == 0) return(NA) else return(tp / (tp + fn))
  })
  mean(rec, na.rm = TRUE)
}

mae_ord <- function(y_true, y_pred) mean(abs(y_true - y_pred))

summarise_metric <- function(x) {
  c(mean = mean(x), sd = sd(x), max = max(x), min = min(x))
}

weighted_kappa_safe <- function(y_true, y_pred, lev, weights = "quadratic") {
  y_true <- factor(y_true, levels = lev, ordered = TRUE)
  y_pred <- factor(y_pred, levels = lev, ordered = TRUE)
  tab <- table(y_true, y_pred)
  n   <- sum(tab)
  K   <- length(lev)
  if (n == 0 || K < 2) return(NA_real_)
  d <- abs(row(tab) - col(tab))
  if (weights == "linear") {
    W <- d / (K - 1)
  } else if (weights == "quadratic") {
    W <- (d / (K - 1))^2
  } else {
    stop("weights must be 'linear' or 'quadratic'")
  }
  O   <- tab / n
  E   <- outer(rowSums(tab), colSums(tab)) / n^2
  den <- sum(W * E)
  if (den == 0) return(NA_real_)
  1 - sum(W * O) / den
}

# -------------------------------------------------------
# Simulation parameters
# -------------------------------------------------------
nruns <- 100
n     <- 1000
prop  <- 0.8
r     <- 10

models  <- c('clm', 'clmm', 'ordfor', 'omerf', 'omerf_clm')
metrics <- c('acc', 'mse', 'rmse', 'mae', 'oc', 'ari', 'ck', 'wk', 'newi', 'precision', 'recall')

runs <- setNames(
  lapply(seq_along(models), function(m)
    setNames(lapply(metrics, function(mt) rep(NA_real_, nruns)), metrics)),
  models)

cms <- setNames(lapply(models, function(m) vector("list", nruns)), models)

omerf.niter     <- rep(NA_integer_, nruns)
omerf.time      <- rep(NA_real_,    nruns)
omerf_clm.niter <- rep(NA_integer_, nruns)
omerf_clm.time  <- rep(NA_real_,    nruns)

cat_counts_list <- vector("list", nruns)

# -------------------------------------------------------
# Main simulation loop
# -------------------------------------------------------
for (nr in 1:nruns) {
  set.seed(nr)
  cat(sprintf("Run %d / %d\n", nr, nruns))

  dati <- build.dataset(n, prop)
  y    <- factor(dati$y.train)
  cov  <- dati$cov.train
  gr   <- factor(dati$group.train)
  lev  <- levels(y)

  cv2 <- do.call(cbind, lapply(1:15, function(lv) as.integer(gr == lv)))
  colnames(cv2) <- paste0('d', 1:15)
  covd <- cbind(cov, cv2)

  clm.data <- data.frame(covd, y)
  clm.mod  <- clm(y ~ x1+x2+x3+x4+x5+x6+x7+
                    d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15,
                  data = clm.data, link = 'logit')

  for.data   <- data.frame(cov, y, gr)
  ordfor.mod <- ordfor(depvar = 'y', perffunction = 'probability', for.data)

  clmm.mod <- clmm(y ~ x1+x2+x3+x4+x5+x6+x7 + (1|gr),
                   link = 'logit', data = for.data, Hess = TRUE,
                   control = clmm.control(maxLineIter = 500,
                                          maxIter = 1000, grtol = 1e-3))

  t0              <- proc.time()["elapsed"]
  omerf.mod       <- omerf(y, cov, gr, toll = 0.05)
  omerf.time[nr]  <- proc.time()["elapsed"] - t0
  omerf.niter[nr] <- omerf.mod$n.iteration
  cat(sprintf("  OMERF:     %d iterations, %.1f s\n", omerf.niter[nr], omerf.time[nr]))

  t0                  <- proc.time()["elapsed"]
  omerf_clm.mod       <- omerf_clm(y, cov, gr, toll = 0.05)
  omerf_clm.time[nr]  <- proc.time()["elapsed"] - t0
  omerf_clm.niter[nr] <- omerf_clm.mod$n.iteration
  cat(sprintf("  OMERF_CLM: %d iterations, %.1f s\n", omerf_clm.niter[nr], omerf_clm.time[nr]))

  y.t    <- factor(dati$y.test)
  cov.t  <- dati$cov.test
  gr.t   <- factor(dati$group.test)
  y_test <- as.numeric(y.t)

  cv2.t <- do.call(cbind, lapply(1:15, function(lv) as.integer(gr.t == lv)))
  colnames(cv2.t) <- paste0('d', 1:15)
  cov.td    <- cbind(cov.t, cv2.t)
  test.data <- data.frame(cov.td, gr = gr.t)

  store_metrics <- function(model_name, y_pred_num, mu_mat) {
    num_classes <- length(lev)
    cm  <- table(y_test, y_pred_num)
    cm2 <- matrix(0, nrow = num_classes, ncol = num_classes)
    rownames(cm2) <- colnames(cm2) <- lev
    cm2[rownames(cm), colnames(cm)] <- cm[rownames(cm), colnames(cm)]
    cms[[model_name]][[nr]] <<- cm2
    runs[[model_name]]$acc[nr]       <<- sum(y_test == y_pred_num) / length(y_test)
    runs[[model_name]]$mse[nr]       <<- mean((y_test - y_pred_num)^2)
    runs[[model_name]]$rmse[nr]      <<- sqrt(mean((y_test - y_pred_num)^2))
    runs[[model_name]]$mae[nr]       <<- mae_ord(y_test, y_pred_num)
    runs[[model_name]]$oc[nr]        <<- OrdinalClassificationIndex(cm2, num_classes)
    runs[[model_name]]$ari[nr]       <<- adj.rand.index(y_test, y_pred_num)
    runs[[model_name]]$ck[nr]        <<- cohen.kappa(cbind(y_test, y_pred_num))$kappa
    runs[[model_name]]$newi[nr]      <<- newindex(mu_mat, y_test, num_classes)$norm.index
    runs[[model_name]]$precision[nr] <<- macro_precision(y_test, y_pred_num, as.numeric(lev))
    runs[[model_name]]$recall[nr]    <<- macro_recall(y_test, y_pred_num, as.numeric(lev))
    runs[[model_name]]$wk[nr]        <<- weighted_kappa_safe(y_test, y_pred_num, as.numeric(lev))
  }

  # CLM predictions
  beta_clm  <- clm.mod$beta
  X_clm     <- as.matrix(test.data[, names(beta_clm), drop = FALSE])
  eta_clm   <- as.numeric(X_clm %*% beta_clm)
  theta_clm <- clm.mod$Theta
  K_clm     <- length(lev)
  cumprob_clm <- matrix(NA_real_, nrow = nrow(test.data), ncol = K_clm - 1)
  for (k in seq_len(K_clm - 1)) cumprob_clm[, k] <- plogis(theta_clm[k] - eta_clm)
  prob_clm      <- matrix(NA_real_, nrow = nrow(test.data), ncol = K_clm)
  prob_clm[, 1] <- cumprob_clm[, 1]
  if (K_clm > 2) for (k in 2:(K_clm - 1)) prob_clm[, k] <- cumprob_clm[, k] - cumprob_clm[, k - 1]
  prob_clm[, K_clm] <- 1 - cumprob_clm[, K_clm - 1]
  prob_clm[prob_clm < 0] <- 0
  prob_clm <- prob_clm / rowSums(prob_clm)
  colnames(prob_clm) <- lev
  mu_clm_t <- as.data.frame(prob_clm)
  y_clm_t  <- as.numeric(lev)[apply(prob_clm, 1, which.max)]
  store_metrics('clm', y_clm_t, mu_clm_t)

  # Ordinal forest predictions
  mu_ord_t <- predict(ordfor.mod, newdata = test.data)$classprobs
  y_ord_t  <- as.numeric(predict(ordfor.mod, newdata = test.data)$ypred)
  store_metrics('ordfor', y_ord_t, mu_ord_t)

  # OMERF predictions
  mu_omerf_t <- predict.omerf(omerf.mod, y, test.data, test.data$gr, type = 'mu')
  y_omerf_t  <- as.numeric(predict.omerf(omerf.mod, y, test.data, test.data$gr, type = 'response'))
  store_metrics('omerf', y_omerf_t, mu_omerf_t)

  # OMERF_CLM predictions
  mu_omerf_clm_t <- predict.omerf_clm(omerf_clm.mod, y, test.data, test.data$gr, type = 'mu')
  y_omerf_clm_t  <- as.numeric(predict.omerf_clm(omerf_clm.mod, y, test.data, test.data$gr, type = 'response'))
  store_metrics('omerf_clm', y_omerf_clm_t, mu_omerf_clm_t)

  # CLMM predictions
  beta      <- clmm.mod$beta
  X_test    <- as.matrix(test.data[, names(beta), drop = FALSE])
  eta_fixed <- as.numeric(X_test %*% beta)
  re     <- ranef(clmm.mod)$gr
  re_vec <- if (is.data.frame(re)) setNames(re[[1]], rownames(re)) else re
  b      <- unname(re_vec[as.character(test.data$gr)])
  b[is.na(b)] <- 0
  eta   <- eta_fixed + b
  theta <- clmm.mod$Theta
  K     <- length(lev)
  cumprob <- matrix(NA_real_, nrow = nrow(test.data), ncol = K - 1)
  for (k in seq_len(K - 1)) cumprob[, k] <- plogis(theta[k] - eta)
  prob      <- matrix(NA_real_, nrow = nrow(test.data), ncol = K)
  prob[, 1] <- cumprob[, 1]
  if (K > 2) for (k in 2:(K - 1)) prob[, k] <- cumprob[, k] - cumprob[, k - 1]
  prob[, K] <- 1 - cumprob[, K - 1]
  prob[prob < 0] <- 0
  prob <- prob / rowSums(prob)
  colnames(prob) <- lev
  mu_clmm_t <- as.data.frame(prob)
  y_clmm_t  <- as.numeric(lev)[apply(prob, 1, which.max)]
  store_metrics('clmm', y_clmm_t, mu_clmm_t)

  # Category / group observation counts
  tbl_train   <- table(cat = y)
  tbl_test    <- table(cat = y.t)
  df_catgr_tr <- as.data.frame(table(cat = y,   group = gr))
  df_catgr_te <- as.data.frame(table(cat = y.t, group = gr.t))
  cnt_marg <- rbind(
    data.frame(run = nr, set = "train", group = "all",
               cat = names(tbl_train), count = as.integer(tbl_train)),
    data.frame(run = nr, set = "test",  group = "all",
               cat = names(tbl_test),  count = as.integer(tbl_test))
  )
  cnt_bygrp <- rbind(
    data.frame(run = nr, set = "train",
               group = as.character(df_catgr_tr$group),
               cat   = as.character(df_catgr_tr$cat),
               count = as.integer(df_catgr_tr$Freq)),
    data.frame(run = nr, set = "test",
               group = as.character(df_catgr_te$group),
               cat   = as.character(df_catgr_te$cat),
               count = as.integer(df_catgr_te$Freq))
  )
  cat_counts_list[[nr]] <- rbind(cnt_marg, cnt_bygrp)

}  # end main loop

# -------------------------------------------------------
# Aggregate results
# -------------------------------------------------------

all_perrun <- do.call(rbind, lapply(metrics, function(mt) {
  do.call(rbind, lapply(models, function(m) {
    data.frame(metric = mt, model = m, run = seq_len(nruns), value = runs[[m]][[mt]])
  }))
}))
write.table(all_perrun, file = sprintf("results_perrun_%d.txt", r),
            row.names = FALSE, quote = FALSE, sep = "\t")

all_summary <- do.call(rbind, lapply(metrics, function(mt) {
  do.call(rbind, lapply(models, function(m) {
    s <- summarise_metric(runs[[m]][[mt]])
    data.frame(metric = mt, model = m, mean = s["mean"], sd = s["sd"],
               max = s["max"], min = s["min"])
  }))
}))
write.table(all_summary, file = sprintf("results_summary_%d.txt", r),
            row.names = FALSE, quote = FALSE, sep = "\t")

cm_perrun <- do.call(rbind, lapply(models, function(m) {
  do.call(rbind, lapply(seq_len(nruns), function(nr) {
    mat <- cms[[m]][[nr]]
    do.call(rbind, lapply(rownames(mat), function(tr) {
      data.frame(model = m, run = nr, true_class = tr,
                 pred_class = colnames(mat), count = as.numeric(mat[tr, ]))
    }))
  }))
}))
write.table(cm_perrun, file = sprintf("results_cm_perrun_%d.txt", r),
            row.names = FALSE, quote = FALSE, sep = "\t")

cm_mean <- do.call(rbind, lapply(models, function(m) {
  mean_mat <- Reduce("+", cms[[m]]) / nruns
  ss_mat   <- Reduce("+", lapply(cms[[m]], function(mat) mat^2))
  sd_mat   <- sqrt((ss_mat - nruns * mean_mat^2) / (nruns - 1))
  do.call(rbind, lapply(rownames(mean_mat), function(tr) {
    data.frame(model = m, true_class = tr, pred_class = colnames(mean_mat),
               mean_count = as.numeric(mean_mat[tr, ]),
               sd_count   = as.numeric(sd_mat[tr, ]))
  }))
}))
write.table(cm_mean, file = sprintf("results_cm_mean_%d.txt", r),
            row.names = FALSE, quote = FALSE, sep = "\t")

omerf_tracking <- data.frame(
  run              = seq_len(nruns),
  omerf.niter      = omerf.niter,
  omerf.time.s     = omerf.time,
  omerf_clm.niter  = omerf_clm.niter,
  omerf_clm.time.s = omerf_clm.time
)
write.table(omerf_tracking, file = sprintf("results_omerf_tracking_%d.txt", r),
            row.names = FALSE, quote = FALSE, sep = "\t")

omerf_tracking_summary <- data.frame(
  metric = c("omerf.niter", "omerf.time.s", "omerf_clm.niter", "omerf_clm.time.s"),
  mean   = c(mean(omerf.niter), mean(omerf.time),
             mean(omerf_clm.niter), mean(omerf_clm.time)),
  sd     = c(sd(omerf.niter), sd(omerf.time),
             sd(omerf_clm.niter), sd(omerf_clm.time))
)
write.table(omerf_tracking_summary,
            file = sprintf("results_omerf_tracking_summary_%d.txt", r),
            row.names = FALSE, quote = FALSE, sep = "\t")

cat_counts_all <- do.call(rbind, cat_counts_list)
write.table(cat_counts_all, file = sprintf("results_cat_counts_%d.txt", r),
            row.names = FALSE, quote = FALSE, sep = "\t")

cat("\nAll results saved.\n")
cat(sprintf("  results_perrun_%d.txt\n", r))
cat(sprintf("  results_summary_%d.txt\n", r))
cat(sprintf("  results_cm_perrun_%d.txt\n", r))
cat(sprintf("  results_cm_mean_%d.txt\n", r))
cat(sprintf("  results_omerf_tracking_%d.txt\n", r))
cat(sprintf("  results_omerf_tracking_summary_%d.txt\n", r))
cat(sprintf("  results_cat_counts_%d.txt\n", r))
