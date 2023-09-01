#' Detect the Number of CPU Cores
#'
#' Attempt to detect the number of CPU cores on the current host.
#' This code is comming from `parallel::detectCores` function.
#'
#' @param all.tests Logical: if true apply all known tests.
#' @param logical Logical: if possible, use the number of physical CPUs/cores (if FALSE) or logical CPUs (if TRUE). Currently this is honoured only on macOS, Solaris and Windows.
#'
#' @return
#' An integer, NA if the answer is unknown.
#' Exactly what this represents is OS-dependent: where possible by default it counts logical (e.g., hyperthreaded) CPUs and not physical cores or packages.
#' Under macOS there is a further distinction between ‘available in the current power management mode’ and ‘could be available this boot’, and this function returns the first.
#' On Sparc Solaris logical = FALSE returns the number of physical cores and logical = TRUE returns the number of available hardware threads. (Some Sparc CPUs have multiple cores per CPU, others have multiple threads per core and some have both.) For example, the UltraSparc T2 CPU in the former CRAN check server was a single physical CPU with 8 cores, and each core supports 8 hardware threads. So detectCores(logical = FALSE) returns 8, and detectCores(logical = TRUE) returns 64.
#' Where virtual machines are in use, one would hope that the result for logical = TRUE represents the number of CPUs available (or potentially available) to that particular VM.
#' @noRd
#'
detectCores <- function (all.tests = FALSE, logical = TRUE)  {
  systems <- list(linux = "grep \"^processor\" /proc/cpuinfo 2>/dev/null | wc -l",
                  darwin = if (logical) "/usr/sbin/sysctl -n hw.logicalcpu 2>/dev/null" else "/usr/sbin/sysctl -n hw.physicalcpu 2>/dev/null",
                  solaris = if (logical) "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l",
                  freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null", openbsd = "/sbin/sysctl -n hw.ncpuonline 2>/dev/null")
  nm <- names(systems)
  m <- pmatch(nm, R.version$os)
  m <- nm[!is.na(m)]
  if (length(m)) {
    cmd <- systems[[m]]
    if (!is.null(a <- tryCatch(suppressWarnings(system(cmd,
                                                       TRUE)), error = function(e) NULL))) {
      a <- gsub("^ +", "", a[1])
      if (grepl("^[1-9]", a))
        return(as.integer(a))
    }
  }
  if (all.tests) {
    for (i in seq(systems)) for (cmd in systems[i]) {
      if (is.null(a <- tryCatch(suppressWarnings(system(cmd,
                                                        TRUE)), error = function(e) NULL)))
        next
      a <- gsub("^ +", "", a[1])
      if (grepl("^[1-9]", a))
        return(as.integer(a))
    }
  }
  NA_integer_
}
