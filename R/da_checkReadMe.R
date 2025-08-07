#' Check a ReadMe in a data archive for data applicants.
#'
#' @description Check if the ReadMe and the list of files (and their names) of a data archive
#'  correspond.
#'
#' @details The ReadMe should list all files that are part of the data archive with their exact
#'  names. At the same time, the data archive should include all files listed in the ReadMe.
#' ### Standard ReadMe name and folder structure
#'  The following standard folder structures (with the ReadMe situated in the last folder) are supported:
#'  \tabular{rl}{1 \tab Q:/FDZ/Alle/02_Antragsverfahren/Verfahren_personalisiert/_2025/2501-01a_Mustermensch/Daten_2025-01-02/2501-01a_SUF_Off-site/Readme_2501-01a_PISA2000.txt}
#'  To add new standard folder structures, please contact the package/function author. Alternatively,
#'  please set the specific \code{path} in this function.
#'
#' @param AID Character string of the application ID in the format of 'YYMM-DDx'
#' @param path Character string of the path to the ReadMe file. If \code{NULL},
#'  the standard folder structure (see details/ \code{struct}) will be assumed.
#' @param fstruct Numeric value corresponding to the standard folder structure described in the
#'  details section.
#'
#' @return Returns a list with the following entries:
#'  \tabular{ll}{
#'  files_not_in_dir \tab Which files are in the ReadMe but not in the directory (incl. subdirectories)? \cr
#'  files_not_in_rm \tab Which files are in the (sub-) directories but not in the ReadMe?}
#'
#' @examples
#' # Basic example
#' da_checkReadMe("2404-10b")
#'
#' @export
da_checkReadMe <- function(AID, path = NULL, fstruct = 1) {
  # check AID
  aid_split <- check_and_split_id(AID, split = TRUE)
  # check path
  if (!(is.character(path) || is.null(path))) {
    stop("'path' must be either NULL or a character string but is of type ", typeof(path),
         call. = FALSE)
  }
  if (is.character(path) && length(path) > 1) {
    stop("'path' must be a single character string (or NULL) but is a character vector of length ", length(path),
         call. = FALSE)
  }
  # check fstruct
  if (!is.numeric(fstruct)) {
    if (is.null(fstruct)) {
      fstruct <- 1
      warning("'fstruct' is NULL but should be a numeric value. fstruct = 1 is assumed.",
              call. = FALSE)
    } else {
      stop("'fstruct' must be a numeric value but is of type ", typeof(fstruct),
           call. = FALSE)
    }
  }
  if (length(fstruct) > 1) {
    stop("'fstruct' must be a single numeric value but is a numeric vector of length ", length(fstruct),
         call. = FALSE)
  }

  path_to_readme <- path
  if (is.null(path_to_readme)) {
    pathstruct <- standardpaths[fstruct]
    pathstruct <- gsub(pattern = "\\[YY\\]",
                       replacement = aid_split$year,
                       x = pathstruct)
    pathstruct <- gsub(pattern = "\\[MM\\]",
                       replacement = aid_split$month,
                       x = pathstruct)
    pathstruct <- gsub(pattern = "\\[DD\\]",
                       replacement = aid_split$day,
                       x = pathstruct)
    pathstruct <- gsub(pattern = "\\[disamb\\]",
                       replacement = aid_split$disamb,
                       x = pathstruct)
    pathsplit <- unlist(strsplit(pathstruct, split = "/"))


    for (folder in pathsplit[1:(length(pathsplit) - 1)]) {
      has_level <- grepl(pattern = "\\[LEVEL\\]",
                         x = folder)
      path_to_readme <- find_directories(foldername = folder,
                                         pathbase = path_to_readme,
                                         accept_dupl = has_level)
    }
  }

  if (grepl(pattern = "\\.txt$", x = path_to_readme[[1]])) {
    fullpath <- path_to_readme
  } else {
    fullpath <- NULL
    for (subdir in path_to_readme) {
      files_on_rm_level <- list.files(path = subdir,
                                      recursive = FALSE,
                                      full.names = FALSE)
      files_on_rm_level <- grep(pattern = "\\.[[:alnum:]]{3,4}$",
                                x = files_on_rm_level,
                                value = TRUE)
      rmpattern = paste0(sub(pattern = "\\[.+$",
                             replacement = "",
                             x = pathsplit[length(pathsplit)]),
                         ".+txt")
      rmname <- grep(pattern = rmpattern,
                     x = files_on_rm_level,
                     value = TRUE,
                     ignore.case = TRUE)
      if (length(rmname) == 0 && paste0("Readme_", AID, ".txt") %in% files_on_rm_level) {
        rmname <- paste0("Readme_", AID, ".txt")
      }
      if (length(rmname) > 1) {
        tmpname <- grep(pattern = pathsplit[length(pathsplit)],
                        x = files_on_rm_level,
                        value = TRUE,
                        ignore.case = TRUE)
        if (length(tmpname) > 1) {
          stop("More than one file matching the ReadMe name found: ", tmpname)
        } else {
          rmname <- tmpname
        }
      }
      fullpath <- c(fullpath, file.path(subdir, rmname))
    }
  }

  returnlist <- list(list(NULL, NULL))
  for (i in seq_along(fullpath)) {
    rmpath <- fullpath[i]
    rmcontent <- readLines(rmpath,
                           encoding = "latin1",
                           warn = FALSE)
    lines_with_files <- grep(pattern = "\\.[[:lower:]]{3}([[:blank:]]|\\\t)+-",
                             x = rmcontent,
                             value = TRUE)
    rm_filelist <- gsub(pattern = "([[:blank:]]|\\\t)+.+$",
                        replacement = "",
                        x = lines_with_files)
    dir_filelist <- list.files(path = dirname(rmpath),
                               recursive = TRUE)
    returnlist[[i]] <- data.frame(files_not_in_dir = rm_filelist[!rm_filelist %in% basename(dir_filelist)],
                          files_not_in_rm = dir_filelist[!basename(dir_filelist) %in% rm_filelist])
    names(returnlist)[[i]] <- basename(rmpath)
  }
  names(returnlist) <- basename(fullpath)
  return(returnlist)
}

check_and_split_id <- function(AID, split = FALSE) {
  if (!is.character(AID)) {
    stop("AID/the application ID needs to be of type character but is of type ", typeof(AID),
         call. = FALSE)
  }
  if (!grepl(pattern = "^[[:digit:]]{4}-[[:digit:]]{2}[[:lower:]]{1}$", x = AID)) {
    stop("AID/the application ID needs to adher to the format 'YYMM-DDx' but is '", AID, "'",
         call. = FALSE)
  }

  if (isTRUE(split)) {
    year <- substr(x = AID, start = 0, stop = 2)
    disamb <- substr(x = AID, start = 8, stop = 8)
  }
  month <- substr(x = AID, start = 3, stop = 4)
  day <- substr(x = AID, start = 6, stop = 7)
  if (!as.numeric(month) %in% 1:12) {
    stop("The month number ('MM' in format 'YYMM-DDx') of the application ID is invalid: ", month,
         call. = FALSE)
  }
  dayspermonth <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (!as.numeric(month) %in% 1:12 | as.numeric(day) > dayspermonth[as.numeric(month)]) {
    stop("The day number ('DD' in format 'YYMM-DDx') of the application ID is invalid: ", day,
         call. = FALSE)
  }

  if (isTRUE(split)) {
    return(list(year = year,
                month = month,
                day = day,
                disamb = disamb))
  }
}

find_directories <- function(foldername, pathbase = NULL, accept_dupl = FALSE) {
  if (is.null(pathbase)) {
    if (dir.exists(foldername) || length(list.dirs(foldername, recursive = FALSE))) {
      return(foldername)
    } else {
      stop("Top level folder '", foldername, "' does not exist.")
    }
  }
  pathout <- NULL
  for (single_base in pathbase) {
    if (dir.exists(file.path(single_base, foldername))) {
      pathout <- c(pathout, file.path(single_base, foldername))
    } else {
      pathcontent <- list.dirs(single_base,
                               recursive = FALSE,
                               full.names = FALSE)
      shortname <- sub(pattern = "\\[.+$",
                       replacement = "",
                       x = foldername)
      foundfolder <- grep(pattern = shortname,
                          x = pathcontent,
                          value = TRUE,
                          ignore.case = TRUE)
      if (length(foundfolder) == 0) {
        stop("No folder matching '", foldername, "' (shortened to '", shortname, "') was found in", single_base, ".")
      }
      if (length(foundfolder) > 1) {
        if (!isTRUE(accept_dupl)) {
          stop("More than one folder matches '", foldername, "' (shortened to '", shortname, "') was found in", single_base, ".")
        }
      }
      pathout <- c(pathout, unlist(lapply(foundfolder, function(x) file.path(pathbase, x))))
    }
  }
  return(pathout)
}
