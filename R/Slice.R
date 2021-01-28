
Slice = function(fix, stim) {
  
  # load packages
  require(zoo)
  
  # convert stimulus information
  line_Y = tapply(stim$ym, stim$line, max)
  lines = 1:length(line_Y)
  lineHeight = mean(diff(line_Y))
  slices = seq(min(stim$xs) + 2 * lineHeight,
               max(stim$xe) - 2 * lineHeight,
               by=lineHeight)
  
  # enumerate runs
  fix$distx <- NA
  fix$distx[2:length(fix$distx)] <- diff(fix$xn)
  fix$disty <- NA
  fix$disty[2:length(fix$disty)] <- diff(fix$yn)
  fix$run[1] <- 1
  for (i in 2:nrow(fix)) {
    if (abs(fix$disty[i]) >= (0.66 * lineHeight) | abs(fix$distx[i]) >= 4 * lineHeight) {
      fix$run[i] <- fix$run[i - 1] + 1
    } else {
      fix$run[i] <- fix$run[i - 1]
    }
  }
  
  s = data.frame(l = c(), x = c(), y = c()) # dataframe for slots
  
  # find intersection with slices
  for (run in 1:max(fix$run)) {
    run = (fix$run == run)
    x = fix$xn[run]
    y = fix$yn[run]
    for (sec in slices) {
      intersects = rollapply(x, 2, function (z) (z[1] < sec & z[2] >= sec |
                                                   z[1] > sec & z[2] <= sec))
      yi = rollapply(y, 2, mean)[intersects]
      xi = rep(sec, length(yi))
      s = rbind(s, data.frame("x" = xi, "y" = yi))
    }
  }
  
  # summarize close intersections
  for (sec in slices) {
    sy = sort(s$y[s$x == sec])
    num_lines = num_lines_at_sec(sec, stim)
    while (length(sy) > 1) { # 
      if (length(sy) <= num_lines) {
        if (min(diff(sy)) > (0.33 * lineHeight)) break
      }
      p = which.min(diff(sy))
      ys = sy[c(p, p+1)]
      s = rbind(s[!(s$y %in% ys & s$x == sec),], data.frame("x" = sec, "y" = mean(ys)))
      sy = sy[-p]
      sy[p] = mean(ys)
    }
  }
  
  # chunk lines
  nr = 1
  for (sec in slices) {
    lefts = s[(sec - s$x) < (2 * lineHeight) & (sec - s$x) > 0,]
    for (y in s$y[s$x == sec]) {
      diffs = abs(y - lefts$y)
      if (length(diffs) > 0) {
        if (min(diffs) < (0.25 * lineHeight)) {
          p = which.min(diffs)
          s$l[s$x == sec & s$y == y] = s$l[s$x == lefts$x[p] & s$y == lefts$y[p]]
        } else {
          s$l[s$x == sec & s$y == y] = nr
          nr = nr + 1
        }
      } else {
        s$l[s$x == sec & s$y == y] = nr
        nr = nr + 1
      }
    }
  }
  for (l in unique(s$l)) {
    p = s[s$l == l, c("x", "y")]
    if (nrow(p) < 2 & sum(p$y < (min(line_Y) + 0.5 * lineHeight)) > 0) {
      s = s[!s$l == l,]
    }
  }
  # reduce to N lines
  lineSizes = as.data.frame(table(s$l))
  linenr = lineSizes$Var1[which.max(lineSizes$Freq)]
  s = addMissing(s, linenr, slices)
  lastUp = nextUp = linenr
  lastDown = nextDown = linenr
  a = 1
  while(length(unique(s$l)) > length(line_Y) | a <= length(lines)) {
    if (nextUp == FALSE & nextDown == FALSE) {
      lineHeight = lineHeight + 1
      nextUp = linenr
      nextDown = linenr
    }
    if (nextUp) {
      lastUp = nextUp
      res = absorbLines(s, nextUp, up=T, lineHeight, slices=slices)
      s = res$slots
      nextUp = res$nextnr
      a = a + 1
    }
    if (nextDown) {
      lastDown = nextDown
      res = absorbLines(s, nextDown, up=F, lineHeight, slices=slices)
      s = res$slots
      nextDown = res$nextnr
      a = a + 1
    }
  }
  
  # assign line number
  means = aggregate(s, list(line = s$l), mean)
  nr = 1
  lnr = rep(NA, nrow(s))
  for (l in means$l[order(means$y)]) {
    lnr[s$l == l] = nr
    nr = nr + 1
  }
  s$l = lnr
  
  # assign fixations
  for (i in 1:nrow(fix)) {
    f = fix[i,]
    m = matrix(c(f$xn, s$x, f$yn, s$y), nrow=nrow(s)+1, ncol=2)
    fix$yn[i] = s$l[which.min(as.matrix(dist(m))[-1,1])]
  }
  return(fix)
}


num_lines_at_sec = function(s, stimmat) {
  return(max(stimmat$line[stimmat$xe >= s & stimmat$xs <= s]))
}

addMissing = function(slots, line, slices) {
  ps = slots[slots$l == line,]
  if (nrow(ps) == 0) return(slots)
  missing = setdiff(slices, ps$x)
  for (m in missing) {
    ps = slots[slots$l == line,]
    left = ps[ps$x < m,][which.min(abs(m - ps$x[ps$x < m])),c("x","y")]
    right = ps[ps$x > m,][which.min(abs(m - ps$x[ps$x > m])),c("x","y")]
    if (nrow(left) > 0 & nrow(right) > 0) {
      y = left$y + (right$y - left$y) / (right$x - left$x) * 50
    } else if (nrow(left) > 0) {
      y = left$y
    } else {
      y = right$y
    }
    slots[nrow(slots) + 1,] = c(m,y,line)
  }
  return(slots)
}

absorbLines = function(slots, line, up = T, lineHeight, t1=0.5, t2=1.4, slices) {
  ul = unique(slots$l)
  l0 = slots[slots$l == line, c("x", "y")]
  if (nrow(l0) == 0) return(list("slots"=slots,"nextnr"=F))
  ls = ul[ul != line]
  ldiffs = data.frame(l = ls, d = rep(NA, length(ls)))
  for (l in ls) {
    xs = intersect(slots$x[slots$l == l], l0$x)
    if (length(xs) == 0) {
      ldiffs$d[ldiffs$l == l] = Inf
      next
    }
    l0ys = aggregate(l0$y, by=list(a = l0$x), FUN=mean)
    l1 = slots[slots$l == l & slots$x %in% xs, c("x", "y")]
    l1ys = aggregate(l1$y, by=list(a = l1$x), FUN=mean)
    ldiffs$d[ldiffs$l == l] = mean(l0ys$x[l0ys$a %in% xs] - l1ys$x)
  }
  idxs = if (up) ldiffs$d <= (lineHeight * t1) & ldiffs$d >= 0 else ldiffs$d >= (lineHeight * -t1) & ldiffs$d <= 0
  idxs[is.na(idxs)] = FALSE
  if (sum(idxs) > 0) {
    neighbours = ldiffs$l[idxs]
    slots$l[slots$l %in% neighbours] = line
  }
  idx = if (up) ldiffs$d < (lineHeight * t2) & ldiffs$d > (lineHeight * t1) else ldiffs$d > (lineHeight * -t2) & ldiffs$d < (lineHeight * t1)
  idx[is.na(idx)] = FALSE
  if (sum(idx) > 0) {
    neighbours = ldiffs$l[idx]
    slots$l[slots$l %in% neighbours] = neighbours[1]
    slots = addMissing(slots, neighbours[1], slices)
  } else {
    neighbours = vector(length=1L)
    neighbours[1] = F
  }
  return(list("slots"=slots,"nextnr"=neighbours[1]))
}
