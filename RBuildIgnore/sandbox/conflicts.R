#'
#' @importFrom purrr set_names keep imap map compact
#' @import cli 
#' 
invert <- function (x) {
  if (length(x) == 0) 
    return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}
#' @export
mosaic_conflicts <- function ()  {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(lapply(envs, ls))
  conflicts <- purrr::keep(objs, ~length(.x) > 1)
  mosaic_names <- paste0("package:", c('mosaic', 'mosaicData', 'mosaicCore'))
  conflicts <- purrr::keep(conflicts, ~any(.x %in% mosaic_names))
  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)
  structure(conflict_funs, class = "package_conflicts")
}

print.package_conflicts <- function(x, ...) {
  cli::cat_line(package_conflict_message(x))
}

package_conflict_message <- function (x) 
{
  if (length(x) == 0) 
    return("")
  header <- cli::rule(left = crayon::bold("Conflicts"), right = "mosaic_conflicts()")
  pkgs <- x %>% purrr::map(~gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- 
    purrr::map2_chr(
      others, names(others), 
      ~paste0(crayon::blue(.x), "::", .y, "()", collapse = ", "))
  winner <- pkgs %>% purrr::map_chr(1)
  funs <- 
    format(
      paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x), "()")))
    )
  bullets <- 
    paste0(crayon::red(cli::symbol$cross), " ", funs, 
           " masks ", other_calls, collapse = "\n")
  paste0(header, "\n", bullets)
}
  

confirm_conflict <-
  function (packages, name) {
    objs <- packages %>% purrr::map(~get(name, pos = .)) %>% 
      purrr::keep(is.function)
    if (length(objs) <= 1) 
      return()
    objs <- objs[!duplicated(objs)]
    packages <- packages[!duplicated(packages)]
    if (length(objs) == 1) 
      return()
    packages
  }

find_conflicts <- function(pkg = 'mosaic') {
  # based on ideas in 
  #   * https://stackoverflow.com/questions/39137110/what-does-the-following-object-is-masked-from-packagexxx-mean
  #   * tidyverse
  envs <- search() %>% setNames(., .)
  
  # For each environment, get the exported functions (and other variables).
  fns <- lapply(envs, ls)
  # Turn this into a data frame, for easy use with dplyr.
  
  fns_by_env <- dplyr::tibble(
    env = rep.int(names(fns), lengths(fns)),
    fn  = unlist(fns)
  )
  
  # Find cases where the object appears more than once.
  fns_by_env %>% 
    dplyr::group_by(fn) %>% 
    dplyr::tally() %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::inner_join(fns_by_env) %>%
    dplry::group_by(fn) %>% 
    dplyr::mutate(keep = grepl(paste0('package:pkg' %in% env))) %>% 
    filter(keep) 
}

