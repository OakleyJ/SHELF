


qhist<-function(q, z, pz){
  quantiles <- rep(NA_real_, length(q))
  quantiles[is.na(q)] <- q[is.na(q)]
  # At probability zero, retain the first supplied edge.
  quantiles[!is.na(q) & q == pz[1]] <- z[1]

  inside <- !is.na(q) & q > pz[1] & q <= pz[length(pz)]
  # Left-open intervals select the first edge reaching q at a CDF plateau.
  # For q above a plateau, interpolation starts after the empty bins.
  i <- findInterval(q[inside], pz, left.open = TRUE)
  fraction <- (q[inside] - pz[i]) / (pz[i + 1] - pz[i])
  quantiles[inside] <- z[i] + fraction * (z[i + 1] - z[i])
  quantiles
}
