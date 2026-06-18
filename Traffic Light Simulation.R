set.seed(3411)

T.horizon <- 10800
warm.up <- 600
capacity <- 55
lost.time <- 4
ped.extra <- 6
NREP <- 30   # fast final run: 30 replications per strategy
SREP <- 10   # fast sensitivity run: 10 replications per demand level
BBOOT <- 200 # bootstrap resamples; could be increased only if the machine is capable

segments <- data.frame(
  start = c(0, 3600, 7200),
  end = c(3600, 7200, 10800),
  main_rate = c(0.11, 0.19, 0.13),
  side_rate = c(0.055, 0.095, 0.065)
)

strategies <- data.frame(
  strategy = c("Equal_35_35", "Short_30_20", "MainPriority_50_25", "LongCycle_70_30", "AdaptivePressure"),
  type = c("fixed", "fixed", "fixed", "fixed", "adaptive"),
  main = c(35, 30, 50, 70, NA),
  side = c(35, 20, 25, 30, NA)
)

exp.rn <- function(n, rate) -log(1 - runif(n)) / rate

make.times <- function(a, b, rate) {
  n <- max(50, ceiling((b - a) * rate * 1.6))
  x <- a + cumsum(exp.rn(n, rate))
  while (max(x) < b) x <- c(x, max(x) + cumsum(exp.rn(n, rate)))
  x[x < b]
}

make.service <- function(n, road) {
  if (road == "main") sample(c(2, 4, 6), n, TRUE, c(0.84, 0.12, 0.04))
  else sample(c(2, 4, 6), n, TRUE, c(0.88, 0.10, 0.02))
}

make.arrivals <- function(mult = 1) {
  main.time <- side.time <- main.serv <- side.serv <- numeric()
  for (i in 1:nrow(segments)) {
    mt <- make.times(segments$start[i], segments$end[i], segments$main_rate[i] * mult)
    st <- make.times(segments$start[i], segments$end[i], segments$side_rate[i] * mult)
    main.time <- c(main.time, mt); side.time <- c(side.time, st)
    main.serv <- c(main.serv, make.service(length(mt), "main"))
    side.serv <- c(side.serv, make.service(length(st), "side"))
  }
  list(main.time = main.time, side.time = side.time, main.serv = main.serv, side.serv = side.serv)
}

get.plan <- function(strategy, qm, qs) {
  s <- strategies[strategies$strategy == strategy, ]
  if (s$type == "fixed") {
    mg <- s$main; sg <- s$side
  } else if (qm >= 1.7 * qs + 8) {
    mg <- 65; sg <- 20
  } else if (qs >= 0.8 * qm + 6) {
    mg <- 35; sg <- 45
  } else if (qm + qs < 12) {
    mg <- 30; sg <- 20
  } else {
    mg <- 50; sg <- 30
  }
  list(phase = c(1, 0, 2, 0),
       dur = c(mg, lost.time + ped.extra * (runif(1) < 0.14),
               sg, lost.time + ped.extra * (runif(1) < 0.18)))
}

q.length <- function(head, tail) ifelse(tail >= head, tail - head + 1, 0)

simulate.one <- function(strategy, mult = 1, details = FALSE) {
  a <- make.arrivals(mult)
  nm <- length(a$main.time); ns <- length(a$side.time)
  ma <- numeric(nm); ms <- numeric(nm); sa <- numeric(ns); ss <- numeric(ns)
  mh <- mt <- sh <- st <- 1
  im <- is <- 1
  wm <- ws <- numeric(0)
  plan <- get.plan(strategy, 0, 0)
  p <- 1; phase <- plan$phase[p]; left <- plan$dur[p]
  service.left <- area.q <- max.q <- spill.sec <- 0
  if (details) trace <- data.frame(time = 0:(T.horizon - 1), q_total = 0, q_main = 0, q_side = 0)

  for (t in 0:(T.horizon - 1)) {
    while (im <= nm && a$main.time[im] <= t) { ma[mt] <- a$main.time[im]; ms[mt] <- a$main.serv[im]; mt <- mt + 1; im <- im + 1 }
    while (is <= ns && a$side.time[is] <= t) { sa[st] <- a$side.time[is]; ss[st] <- a$side.serv[is]; st <- st + 1; is <- is + 1 }

    qm <- q.length(mh, mt - 1); qs <- q.length(sh, st - 1)
    if (left <= 0) {
      p <- p + 1
      if (p > 4) { plan <- get.plan(strategy, qm, qs); p <- 1 }
      phase <- plan$phase[p]; left <- plan$dur[p]; service.left <- 0
    }

    if (service.left <= 0 && phase == 1 && qm > 0) {
      if (ma[mh] >= warm.up) wm <- c(wm, t - ma[mh])
      service.left <- ms[mh]; mh <- mh + 1
    }
    if (service.left <= 0 && phase == 2 && qs > 0) {
      if (sa[sh] >= warm.up) ws <- c(ws, t - sa[sh])
      service.left <- ss[sh]; sh <- sh + 1
    }
    if (phase != 0) service.left <- service.left - 1 else service.left <- 0

    qm <- q.length(mh, mt - 1); qs <- q.length(sh, st - 1)
    tq <- qm + qs
    area.q <- area.q + tq; max.q <- max(max.q, tq); spill.sec <- spill.sec + (tq > capacity)
    if (details) trace[t + 1, 2:4] <- c(tq, qm, qs)
    left <- left - 1
  }

  w <- c(wm, ws)
  m <- function(x) if (length(x)) mean(x) else NA
  q <- function(x, p) if (length(x)) as.numeric(quantile(x, p)) else NA
  ans <- list(
    strategy = strategy, demand_mult = mult, served = length(w),
    left_main = q.length(mh, mt - 1), left_side = q.length(sh, st - 1),
    avg_wait = m(w), median_wait = q(w, 0.50), p90_wait = q(w, 0.90), p95_wait = q(w, 0.95),
    max_wait = if (length(w)) max(w) else NA,
    avg_main_wait = m(wm), avg_side_wait = m(ws), fairness_gap = abs(m(wm) - m(ws)),
    prob_wait_gt_120 = if (length(w)) mean(w > 120) else NA,
    time_avg_queue = area.q / T.horizon, max_queue = max.q, spillback_risk = spill.sec / T.horizon
  )
  if (details) { ans$trace <- trace; ans$waits <- data.frame(wait = w, direction = c(rep("main", length(wm)), rep("side", length(ws)))) }
  ans
}

run.experiment <- function(n.rep = NREP, mult = 1) {
  out <- vector("list", nrow(strategies) * n.rep); k <- 1
  for (s in strategies$strategy) {
    cat("Running", s, "at demand multiplier", mult, "...\n")
    flush.console()
    for (r in 1:n.rep) {
      z <- simulate.one(s, mult)
      out[[k]] <- data.frame(z, check.names = FALSE)
      k <- k + 1
    }
  }
  do.call(rbind, out)
}

boot.ci <- function(x) as.numeric(quantile(replicate(BBOOT, mean(sample(x, length(x), TRUE))), c(0.025, 0.975)))

summary.stats <- function(res) {
  out <- data.frame()
  for (s in strategies$strategy) {
    d <- res[res$strategy == s, ]
    ci <- mean(d$avg_wait) + c(-1, 1) * 1.96 * sd(d$avg_wait) / sqrt(nrow(d))
    bi <- boot.ci(d$avg_wait)
    score <- mean(d$avg_wait) + 0.35 * mean(d$p90_wait) + 20 * mean(d$prob_wait_gt_120) +
      0.20 * mean(d$fairness_gap) + 50 * mean(d$spillback_risk)
    out <- rbind(out, data.frame(
      strategy = s, mean_wait = mean(d$avg_wait), ci_low = ci[1], ci_high = ci[2],
      boot_low = bi[1], boot_high = bi[2], mean_p90 = mean(d$p90_wait), mean_p95 = mean(d$p95_wait),
      mean_main_wait = mean(d$avg_main_wait), mean_side_wait = mean(d$avg_side_wait),
      fairness_gap = mean(d$fairness_gap), prob_gt_120 = mean(d$prob_wait_gt_120),
      time_avg_queue = mean(d$time_avg_queue), max_queue = mean(d$max_queue),
      spillback_risk = mean(d$spillback_risk), served = mean(d$served),
      left_total = mean(d$left_main + d$left_side), score = score
    ))
  }
  out[order(out$score), ]
}

sensitivity <- function(mult = c(0.75, 0.90, 1.00, 1.10, 1.25, 1.40)) {
  out <- data.frame()
  for (m in mult) {
    cat("\nSensitivity demand multiplier", m, "\n")
    flush.console()
    res <- run.experiment(SREP, m)
    for (s in strategies$strategy) {
      d <- res[res$strategy == s, ]
      out <- rbind(out, data.frame(strategy = s, demand_mult = m, mean_wait = mean(d$avg_wait)))
    }
  }
  out
}

wide.sensitivity <- function(x) {
  y <- reshape(x, idvar = "demand_mult", timevar = "strategy", direction = "wide")
  names(y) <- sub("^mean_wait\\.", "", names(y))
  y[order(y$demand_mult), ]
}

round.print <- function(x, digits = 3) {
  y <- x; nums <- sapply(y, is.numeric); y[nums] <- lapply(y[nums], round, digits)
  print(y, row.names = FALSE)
}

nice <- function(x) gsub("_", " ", x)

plot.fig1 <- function(tab) {
  old <- par(no.readonly = TRUE); on.exit(par(old)); par(mar = c(8, 4, 4, 2))
  x <- barplot(tab$mean_wait, names.arg = nice(tab$strategy), las = 2,
               ylab = "Average waiting time (seconds)", main = "Figure 1. Average Waiting Time by Strategy")
  arrows(x, tab$ci_low, x, tab$ci_high, angle = 90, code = 3, length = 0.05)
  grid(nx = NA, ny = NULL)
}

plot.fig2 <- function(tab) {
  old <- par(no.readonly = TRUE); on.exit(par(old)); par(mar = c(8, 4, 4, 2))
  vals <- rbind(tab$mean_p90, tab$time_avg_queue * 10)
  colnames(vals) <- nice(tab$strategy)
  barplot(vals, beside = TRUE, las = 2, col = gray.colors(2), ylab = "Seconds / scaled vehicles",
          main = "Figure 2. High-Percentile Wait and Queue Burden")
  legend("topright", c("P90 waiting time", "Time-average queue x10"), fill = gray.colors(2), bty = "n")
  grid(nx = NA, ny = NULL)
}

plot.fig3 <- function(a, b) {
  plot(a$time / 60, a$q_total, type = "l", xlab = "Time (minutes)", ylab = "Total queue length",
       main = "Figure 3. Queue Length Over One Simulated Period")
  lines(b$time / 60, b$q_total, lty = 2); abline(h = capacity, lty = 3)
  legend("topleft", c("Equal 35/35", "Adaptive Pressure", "Spillback threshold"), lty = c(1, 2, 3), bty = "n")
  grid()
}

plot.fig4 <- function(a, b) {
  xmax <- min(max(a$wait, b$wait, na.rm = TRUE), as.numeric(quantile(a$wait, 0.98, na.rm = TRUE)) + 20)
  plot(ecdf(a$wait), xlim = c(0, xmax), xlab = "Waiting time (seconds)", ylab = "F_n(x)",
       main = "Figure 4. ECDF of Individual Vehicle Waiting Times")
  lines(ecdf(b$wait), lty = 2)
  legend("bottomright", c("Equal 35/35", "Adaptive Pressure"), lty = c(1, 2), bty = "n")
  grid()
}

plot.fig5 <- function(res, tab) {
  old <- par(no.readonly = TRUE); on.exit(par(old)); par(mar = c(8, 4, 4, 2))
  res$strategy <- factor(res$strategy, levels = tab$strategy)
  boxplot(avg_wait ~ strategy, data = res, las = 2, ylab = "Average wait per replication",
          main = "Figure 5. Monte Carlo Distribution Across Replications")
  grid(nx = NA, ny = NULL)
}

plot.fig6 <- function(sens) {
  ylim <- range(sens$mean_wait, na.rm = TRUE)
  for (i in seq_along(strategies$strategy)) {
    s <- strategies$strategy[i]; d <- sens[sens$strategy == s, ]
    d <- d[order(d$demand_mult), ]
    if (i == 1) plot(d$demand_mult, d$mean_wait, type = "b", ylim = ylim,
                     xlab = "Demand multiplier", ylab = "Average waiting time (seconds)",
                     main = "Figure 6. Sensitivity Analysis")
    else lines(d$demand_mult, d$mean_wait, type = "b")
  }
  legend("topleft", nice(strategies$strategy), lty = 1, pch = 1, bty = "n", cex = 0.8)
  grid()
}

start.time <- Sys.time()
cat("Starting traffic-light simulation...\n")
main.results <- run.experiment()
summary.table <- summary.stats(main.results)
equal.detail <- simulate.one("Equal_35_35", details = TRUE)
adaptive.detail <- simulate.one("AdaptivePressure", details = TRUE)
sens.table <- sensitivity()
sens.wide <- wide.sensitivity(sens.table)

cat("\nTable 4. Main performance results\n")
round.print(summary.table[, c("strategy", "mean_wait", "ci_low", "ci_high", "mean_p90", "mean_p95", "score")])

cat("\nTable 5. Fairness, queue, and risk results\n")
round.print(summary.table[, c("strategy", "mean_main_wait", "mean_side_wait", "fairness_gap", "prob_gt_120", "time_avg_queue", "max_queue", "spillback_risk")])

cat("\nTable 6. Bootstrap and normal confidence intervals\n")
round.print(summary.table[, c("strategy", "mean_wait", "boot_low", "boot_high", "ci_low", "ci_high")])

cat("\nTable 7. Mean waiting time under different demand multipliers\n")
round.print(sens.wide)

plot.fig1(summary.table)
plot.fig2(summary.table)
plot.fig3(equal.detail$trace, adaptive.detail$trace)
plot.fig4(equal.detail$waits, adaptive.detail$waits)
plot.fig5(main.results, summary.table)
plot.fig6(sens.table)

cat("\nTotal run time:", round(as.numeric(difftime(Sys.time(), start.time, units = "mins")), 2), "minutes\n")