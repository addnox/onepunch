#' @export
use_locale <- function(locale = c("Chn", "Eng")) {
  locale <- match.arg(locale)
  current_locale <- Sys.getlocale(category = "LC_CTYPE")
  locale_new <- ifelse(locale == "Chn", "Chinese (Simplified)", "English_United States.1252")
  Sys.setlocale(category = "LC_CTYPE", locale = locale_new)
  current_locale
}

#' @export
use_options <- function(set = c("op")) {
  set <- match.arg(set)

  if (set == "op") {
    options(
      "datatable.print.nrows" = 30, # 100
      "datatable.print.class" = TRUE,
      "datatable.print.trunc.cols" = TRUE
    )
  }

  invisible(TRUE)
}

#' Create common directory structure for analysis
#'
#' @export
use_common_dir <- function(wd = NULL, flavor = c("concise", "gc"), create_project = TRUE) {
  flavor <- match.arg(flavor)
  if (is.null(wd)) wd <- getwd()

  old_project <- usethis::proj_set(wd, force = TRUE)

  dir_op <- c(
    "fig",
    "data/raw",
    "data/modeling",
    "data/ref",
    "data/interim",
    "scripts", # where to put R scripts, usually with interactive use
    "src",     # where to put R functions for later sourcing
    "tests",
    "deliverable" # move deliverable out of data/ folder
  )

  dir_gc <- c(
    "Actuarial/Experience",
    "Actuarial/Exposure",
    "Actuarial/Ground-Up LR",
    "CatModeling/Exposure/DataLink",
    "CatModeling/Exposure/DataPrep",
    "CatModeling/Exposure/Information",
    "CatModeling/Models/AIR/Databases",
    "CatModeling/Models/AIR/Input",
    "CatModeling/Models/AIR/Output",
    "CatModeling/Models/APOTH/Databases",
    "CatModeling/Models/APOTH/Input",
    "CatModeling/Models/APOTH/Output",
    "CatModeling/Models/EQECAT/Databases",
    "CatModeling/Models/EQECAT/Input",
    "CatModeling/Models/EQECAT/Output",
    "CatModeling/Models/RMS/Databases",
    "CatModeling/Models/RMS/Input",
    "CatModeling/Models/RMS/Output",
    "CatModeling/SpecialRequests",
    "CatModeling/Working/Reports",
    "Deliverables/FTP",
    "Deliverables/ROLePlay",
    "Documentation/Correspondings",
    "Financial/RiskTransfer",
    "Metarisk",
    "OriginalData"
  )

  if (flavor == "concise") {
    lapply(dir_op, dir_make, wd = wd)
  } else if (flavor == "gc") {
    lapply(dir_gc, dir_make, wd = wd)
    create_project <- FALSE ## GC flavor is usually used for U drive dir creation, so no R project needed
  }

  if (create_project & length(fs::dir_ls(wd, regex = "[.]Rproj$")) == 0) {
    on.exit(usethis::proj_set(old_project), add = TRUE)
    usethis::use_rstudio()
  }

  invisible(usethis::proj_get())
}

