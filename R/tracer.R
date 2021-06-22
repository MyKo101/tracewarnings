#' @export
warning_trace <- function(){
  tr <- rev(sys.calls())[-(1:5)]
  tr2 <- vector("pairlist",length=length(tr))
  for(i in 1:length(tr)){
    tr2[[i]] <- deparse(tr[[i]])
    attributes(tr2[[i]]) <- attributes(tr[[i]])
  }

  .warning_traceback <<- tr2
}

#' @export
.tracebackW <- function() .warning_traceback

#' @export
tracebackW <- function(){
  n <- length(x <- .tracebackW())
  if (n == 0L) {
    cat(gettext("No traceback available"), "\n")
  } else {
    for (i in 1L:n) {
      xi <- x[[i]]
      label <- paste0(n - i + 1L, ": ")
      m <- length(xi)
      srcloc <- if (!is.null(srcref <- attr(xi, "srcref"))) {
        srcfile <- attr(srcref, "srcfile")
        paste0(" at ", basename(srcfile$filename),
               "#", srcref[1L])
      }
      if (isTRUE(attr(xi, "truncated"))) {
        xi <- c(xi, " ...")
        m <- length(xi)
      }
      if (!is.null(srcloc)) {
        xi[m] <- paste0(xi[m], srcloc)
      }
      if (m > 1) {
        label <- c(label, rep(substr("          ",
                                     1L, nchar(label, type = "w")), m - 1L))
      }
      cat(paste0(label, xi), sep = "\n")
    }
  }
  invisible(x)
}



