### ------------------------------------------------------------------------ ###
### ------------------------------ FUNCTIONS ------------------------------- ###
### ------------------------------------------------------------------------ ###

# Flag participants with invalid responses or missing all items on a scale
missing_inval <- function(x, measure = c('base', 'scoff', 'cia', 'BMI_inval', 'EDDS_inval')){
  measure <- match.arg(measure)
  if(endsWith(measure, 'inval')){
    if(!'exclude' %in% colnames(x)){x$exclude <- 0}
    ids <- x$id[which(x[, measure] == 1)]
    x[x$id %in% ids, 'exclude'] <- 1
    message(paste0('IDs with ', measure, ': ', paste0(ids, collapse = ', ')))
    return(x)
  }
  cols <- get_cols(x, measure)
  na <- apply(x[, cols], 1, function(z) sum(is.na(z)))
  if(any(na == length(cols))){
    ids <- x$id[which(na == length(cols))]
    message(paste0('IDs missing all values on the ', measure, ': ', paste0(ids, collapse = ', ')))
    if(!'exclude' %in% colnames(x)){x$exclude <- 0}
    x[x$id %in% ids, 'exclude'] <- 1
  } else {
    message(paste0('No participants missing all values on the ', measure))
  }
  return(x)
}


# Get column names for a scale
get_cols <- function(x, measure = c('base', 'scoff', 'cia')){
  measure <- match.arg(measure)
  total <- switch(2 - isTRUE(measure == 'base'), c('base_9item', 'base_10item'), paste0(measure, '_total'))
  cols <- setdiff(colnames(x)[grep(measure, colnames(x))], total)
  return(cols)
}


# ROC/PRC Function:
ROCcurve <- function(y, X = NULL, model = NULL, plot = FALSE, optPoint = TRUE,
                     grid = FALSE, grid_lty = 3, grid_lwd = 1.5, grid_col = "lightgray",
                     midline = TRUE, midline_lty = 2, midline_lwd = 2, midline_col = "red",
                     pt_pch = 23, pt_border = "black", pt_col = "green", thresh = TRUE,
                     roc_lty = 1, roc_lwd = 2, roc_col = "black", prc = FALSE,
                     plot_na = FALSE, cutoff = '>'){
  cutoff <- match.arg(cutoff, c('>', '>='))
  stopifnot(all(sapply(list(prc, plot, plot_na), is.logical)))
  if(is(y, 'ROCcurve')){
    sens <- y$results$sens
    spec <- y$results$spec
    ppv <- y$results$ppv
    opt <- ifelse(prc, which.max(ppv + sens), which.max(sens + spec))
  } else {
    if(!missing(y) & is.null(model)){
      if(is(y, 'glm')){
        model <- y
        y <- unname(model$y)
      }
    } else if(!is.null(model) & missing(y)){
      y <- unname(model$y)
    }
    stopifnot(dim(table(y)) == 2)
    if(is.factor(y) | is.character(y)){
      y <- factor(y)
      levels(y) <- 0:1
      y <- as.numeric(as.character(y))
    }
    stopifnot(all(names(table(y)) %in% c("0", "1")))
    if(is.null(model)){
      stopifnot(!is.null(X))
      X <- as.data.frame(X)
      model <- glm(y ~ ., data = X, family = binomial)
      predProbs <- predict(model, type = "response")
    } else if(is(model, 'train')){
      predProbs <- predict(model, X, 'prob')[, 2]
    } else if(is(model, 'glinternet')){
      predProbs <- predict(model, X, type = 'response')[, 2]
    } else if(is(model, 'glm') | is(model, 'glmboost') | is(model, 'gbm')){
      predProbs <- predict(model, type = "response")
    } else if(is(model, 'numeric')){
      stopifnot(length(model) == length(y))
      predProbs <- model
    }
    p <- unname(sort(predProbs))
    if(thresh | prc){
      t1 <- (c(-Inf, p) + c(p, +Inf))/2
      t2 <- (c(-Inf, p)/2 + c(p, +Inf)/2)
      p <- ifelse(abs(t1) > 1e+100, t2, t1)
    }
    preds <- list()
    tp <- tn <- fp <- fn <- c()
    for(i in 1:length(p)){
      if(cutoff == '>'){
        preds[[i]] <- ifelse(predProbs > p[i], 1, 0)
      } else if(cutoff == '>='){
        preds[[i]] <- ifelse(predProbs >= p[i], 1, 0)
      }
      Y <- cbind(y, preds[[i]])
      tn[i] <- sum(Y[Y[, 1] == 0, 1] == Y[Y[, 1] == 0, 2])
      tp[i] <- sum(Y[Y[, 1] == 1, 1] == Y[Y[, 1] == 1, 2])
      fn[i] <- sum(Y[Y[, 2] == 0, 1] != Y[Y[, 2] == 0, 2])
      fp[i] <- sum(Y[Y[, 2] == 1, 1] != Y[Y[, 2] == 1, 2])
    }
    sens <- tp/sum(y)
    spec <- tn/(length(y) - sum(y))
    npv <- tn/(tn + fn)
    ppv <- tp/(tp + fp)
    opt <- ifelse(prc, which.max(ppv + sens), which.max(sens+spec))
    optCut <- p[opt]
    optSens <- sens[opt]
    optSpec <- spec[opt]
    optPPV <- ppv[opt]
    optNPV <- npv[opt]
  }
  if(prc){
    sx <- c(1, sens)
    sy <- c(0, ppv)
    syna <- 0
    if(any(is.na(sy))){
      if(sum(is.na(sy)) == 1){
        sy <- na.omit(sy)
      } else {
        syna <- which(is.na(sy))
        sy[syna] <- sx[syna]
      }
    }
    syextra <- length(sy) < length(sx)
    if(syextra){sy <- c(sy, 1)}
    height <- sy[-1] - sy[-length(sy)]
    width <- (sx[-1] + sx[-length(sx)])/2
  } else {
    sx <- c(0, spec)
    sy <- c(1, sens)
    height <- (sy[-1] + sy[-length(sy)])/2
    width <- sx[-1] - sx[-length(sx)]
  }
  AUC <- sum(height * width)
  if(!plot){
    if(is(y, 'ROCcurve')){
      p <- y$results$cutoff
      npv <- y$results$npv
      optCut <- p[opt]
      optSens <- sens[opt]
      optSpec <- spec[opt]
      optPPV <- ppv[opt]
      optNPV <- npv[opt]
    }
    out <- list(results = data.frame(cutoff = p, sens = sens, spec = spec, ppv = ppv, npv = npv),
                optimal = unlist(list(cutoff = optCut, sensitivity = optSens,
                                      specificity = optSpec, PPV = optPPV,
                                      NPV = optNPV)), AUC = AUC)
    class(out) <- c('ROCcurve', 'list')
    attr(out, 'type') <- ifelse(prc, 'PRC', 'ROC')
    return(out)
  } else {
    xlabel <- ifelse(prc, 'Recall', '1 - Specificity')
    ylabel <- ifelse(prc, 'Precision', 'Sensitivity')
    main <- ifelse(prc, paste0('PR Curve\nAUC = ', round(AUC, 3)),
                   paste0('ROC Curve\nAUC = ', round(AUC, 3)))
    plot(0, 0, type = "n", ylim = c(0, 1), xlim = c(0, 1), axes = FALSE,
         xlab = xlabel, ylab = ylabel, main = main)
    if(grid != FALSE){
      if(grid == TRUE){grid <- 1}
      if(grid == 1){
        grid(NA, 5, lty = grid_lty, lwd = grid_lwd, col = grid_col)
      } else if(grid == 2){
        grid(lty = grid_lty, lwd = grid_lwd, col = grid_col)
      }
    }
    axis(1); axis(2)
    if(!plot_na & prc){
      if(syextra){
        sy <- sy[-length(sy)]
        sx <- sx[-length(sx)]
      }
      if(!identical(syna, 0)){
        sx[syna] <- NA
        sy[syna] <- NA
      }
    }
    if(prc){sx <- 1 - sx}
    lines(1 - sx, sy, lty = roc_lty, lwd = roc_lwd, col = roc_col)
    if(midline != FALSE){
      if(midline == TRUE){midline <- 1}
      if(midline == "grey" | midline == "gray"){midline_lty <- 1; midline_col = "grey"}
      if(midline == 2){midline_lty <- 1; midline_col = "grey"}
      prctrue <- as.numeric(prc)
      abline(a = prctrue, b = 1 - (2 * prctrue), lty = midline_lty,
             lwd = midline_lwd, col = midline_col)
    }
    if(optPoint != FALSE){
      if(optPoint == "black" | optPoint == 2){pt_pch <- 8; pt_col <- "black"}
      if(!pt_pch %in% c(21:25)){pt_border <- pt_col}
      points((1 - sx)[opt + 1], sy[opt + 1], pch = pt_pch, bg = pt_col, col = pt_border)
    }
  }
}


# find threshold for a given scale
thresh <- function(x, y, prc = FALSE, scale.max = 'default', cutoff = '>', round = 5){
  stopifnot(length(scale.max) == 1)
  if(identical(scale.max, 'default')){scale.max <- max(x)}
  values <- 0:scale.max
  dat <- data.frame(y, x)
  b <- coef(glm(y ~ x, dat, family = binomial))
  curve <- ROCcurve(y = y, X = x, plot = FALSE, prc = prc, cutoff = cutoff)
  predprobs <- function(v, b){
    pp <- b[1] + b[2] * v
    return(unname(exp(pp)/(1 + exp(pp))))
  }
  if(curve$optimal[1] %in% c(-Inf, Inf)){
    out <- curve$optimal[1]
  } else {
    k <- round(curve$optimal[1], round)
    pp <- round(sapply(values, predprobs, b = b), round)
    out <- values[ifelse(any(pp < k), max(which(pp < k)), 0) + 1]
  }
  names(out) <- paste0(ifelse(prc, 'PRC', 'ROC'), '_thresh')
  return(out)
}


# Compare two AUCs with bootstrapping
auc_test <- function(data, y, x1, x2, nboot = 1000, seed = NULL,
                     prc = FALSE, stratified = TRUE, verbose = TRUE,
                     alternative = c('two.sided', 'less', 'greater'),
                     cutoff = '>'){
  if(is(data, 'list')){
    call <- as.list(match.call())[-1]
    out <- do.call(rbind, lapply(data, function(i){
      calli <- replace(call, 'data', list(i))
      do.call(auc_test, calli)
    }))
    if(!is.null(names(data)) & !identical(prc, 'both')){
      rownames(out) <- names(data)
    }
    return(out)
  } else if(identical(prc, 'both')){
    call <- as.list(match.call())[-1]
    out <- do.call(rbind, lapply(c(FALSE, TRUE), function(i){
      calli <- replace(call, 'prc', i)
      do.call(auc_test, calli)
    }))
    return(out)
  }
  stopifnot(is.character(y) & is.character(x1) & is.character(x2))
  stopifnot(length(unique(data[, y])) == 2)
  alternative <- match.arg(alternative)
  auc1 <- ROCcurve(data[, y], data[, x1], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
  auc2 <- ROCcurve(data[, y], data[, x2], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
  if(!is.null(seed)){set.seed(seed)}
  if(verbose){pb <- txtProgressBar(max = nboot, style = 3)}
  auc_diffs <- sapply(1:nboot, function(i){
    if(stratified){
      k <- unique(data[, y])
      y0 <- data[which(data[, y] == k[1]), ]
      y1 <- data[which(data[, y] == k[2]), ]
      dati <- rbind(y0[sample(1:nrow(y0), replace = TRUE), ],
                    y1[sample(1:nrow(y1), replace = TRUE), ])
    } else {
      dati <- data[sample(1:nrow(data), replace = TRUE), ]
    }
    a1 <- ROCcurve(dati[, y], dati[, x1], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
    a2 <- ROCcurve(dati[, y], dati[, x2], plot = FALSE, prc = prc, cutoff = cutoff)$AUC
    if(verbose){setTxtProgressBar(pb, i)}
    return(a1 - a2)
  })
  d <- (auc1 - auc2)/sd(auc_diffs)
  if(alternative == 'two.sided'){
    p <- pnorm(abs(d), lower.tail = FALSE) * 2
  } else {
    p <- pnorm(d, lower.tail = isTRUE(alternative == 'less'))
  }
  out <- data.frame(type = ifelse(prc, 'PRC', 'ROC'), auc1, auc2,
                    sd_diff = sd(auc_diffs), D = d, pvalue = p)
  attributes(out)[c('nboot', 'stratified', 'alternative')] <- list(nboot, stratified, alternative)
  colnames(out)[startsWith(colnames(out), 'auc')] <- c(x1, x2)
  return(out)
}




# Reformat ROC/PRC output for plotting
ggdata <- function(models, levels = c('scoff_total', 'base_10item', 'base_9item', 'cia_total')){
  sens <- lapply(lapply(lapply(models, '[[', 'results'), '[[', 'sens'), function(z) c(1, z))[levels]
  ppv <- lapply(lapply(lapply(models, '[[', 'results'), '[[', 'ppv'), function(z) c(0, z))[levels]
  spec <- lapply(lapply(lapply(models, '[[', 'results'), '[[', 'spec'), function(z) c(0, z))[levels]
  labels <- sapply(levels, switch, scoff_total = 'SCOFF', base_10item = 'BASE-10',
                   base_9item = 'BASE-9', cia_total = 'CIA')
  n <- unique(sapply(sens, length))
  stopifnot(length(n) == 1)
  names <- factor(rep(names(sens), each = n), levels = levels, labels = labels)
  out <- data.frame(Measure = names,
                    Sensitivity = unlist(sens),
                    Precision = unlist(ppv),
                    Specificity = unlist(spec))
  class(out) <- c('ggdata', 'data.frame')
  return(out)
}

# plot the ROC/PRC results
makePlot <- function(x, type = c('roc', 'prc'), gender = FALSE,
                     title = TRUE, theme = c('bw', 'classic'),
                     levels = c('scoff_total', 'base_10item', 'base_9item', 'cia_total')){
  type <- match.arg(tolower(type), c('roc', 'prc'))
  theme <- match.arg(tolower(theme), c('bw', 'classic'))
  if(!identical(gender, FALSE)){
    if(isTRUE(gender)){gender <- gsub('man$', 'men', names(x))}
    x <- lapply(x, ggdata, levels = levels)
    x[[1]]$gender <- gender[1]
    x[[2]]$gender <- gender[2]
    x <- do.call(rbind, x)
    x$gender <- factor(x$gender)
  }
  if(!is(x, 'ggdata')){x <- ggdata(models = x, levels = levels)}
  if(type == 'roc'){
    g <- ggplot(x, aes(x = 1 - Specificity, y = Sensitivity, color = Measure)) +
      geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1, alpha = .3)
  } else {
    g <- ggplot(x, aes(x = Sensitivity, y = Precision, color = Measure)) +
      geom_abline(slope = -1, intercept = 1, linetype = 'dashed', size = 1, alpha = .3) +
      xlab('Recall (Sensitivity)')
  }
  if(theme == 'classic'){
    g <- g + geom_line(size = 1) + theme_classic()
  } else {
    g <- g + geom_line(size = 1) + theme_bw()
  }
  if(!identical(title, FALSE)){
    if(isTRUE(title)){title <- ifelse(type == 'roc', 'ROC Curve', 'PR Curve')}
    g <- g + ggtitle(title)
  }
  if(!identical(gender, FALSE)){
    g <- g + facet_grid(. ~ gender)
  }
  return(g)
}



# Report descriptive statistics for each scale
descriptives <- function(data, prc, prc_auc, thresholds, scoff_thresh = 2){
  out <- data.frame(AUC = prc_auc, do.call(rbind, lapply(prc, '[[', 'optimal')))
  out[, 'cutoff'] <- thresholds
  scoff <- glm(ED_dx ~ scoff_total, data = data, family = binomial)
  b <- coef(scoff)
  yhat <- b[1] + b[2] * scoff_thresh
  yhat <- exp(yhat)/(1 + exp(yhat))
  scoff_mod2 <- prc$scoff_total$results[which(prc$scoff_total$results$cutoff == yhat), ][1, ]
  scoff_mod2 <- structure(data.frame(AUC = prc_auc['scoff_total'], scoff_mod2), row.names = 'scoff_total2')
  scoff_mod2[1, 'cutoff'] <- scoff_thresh
  colnames(scoff_mod2) <- colnames(out)
  out <- data.frame(rbind(rbind(out[1, ], scoff_mod2), out[-1, ]))
  return(out)
}

# Find summary statistics for different thresholds
testThresh <- function(data, x, thresh){
  y <- data$ED_dx
  stopifnot(x %in% 1:3)
  X <- data[, x + 1]
  p <- ifelse(X > thresh, 1, 0)
  tp <- sum(p == 1 & y == 1)
  tn <- sum(p == 0 & y == 0)
  fp <- sum(p == 1 & y == 0)
  fn <- sum(p == 0 & y == 1)
  sens <- tp/(tp + fn)
  spec <- tn/(tn + fp)
  prec <- tp/(tp + fp)
  npv <- tn/(tn + fn)
  out <- data.frame(sens, spec, prec, npv)
  rownames(out) <- colnames(data)[x + 1]
  return(out)
}
