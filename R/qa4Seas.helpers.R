##     qa4seas.helpers Miscellaneous internals to aid in the application of metaclipR to QA4Seas products
##
##     Copyright (C) 2018 Predictia (http://www.predictia.es)
##
##     This program is free software: you can redistribute it and/or modify
##     it under the terms of the GNU General Public License as published by
##     the Free Software Foundation, either version 3 of the License, or
##     (at your option) any later version.
##
##     This program is distributed in the hope that it will be useful,
##     but WITHOUT ANY WARRANTY; without even the implied warranty of
##     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
##
##     You should have received a copy of the GNU General Public License
##     along with this program.  If not, see <http://www.gnu.org/licenses/>.



#' @title Parse product forecast time
#' @description Extracts the season from the parameters read from the yml file
#' @param init Character string, of the form \code{"fcXXXX"}, as read from the parameter file
#' @param season Character string, as read from the \code{'forecast_time_intervals'} product parameter
#' @param several.ok Logical flag indicating whether the product is displaying several forecast
#'  times in the same plot (e.g., and ENSO plume) or not (e.g., a skill map)
#' @return A list, whose length is equal to the number of products to be generated
#' @importFrom magrittr %>%
#' @keywords internal

parseSeason <- function(init, season, several.ok = TRUE) {
    pplyfun <- ifelse(several.ok, sapply, lapply)
    init.month <- substr(init, 3,4) %>% as.integer()
    out <- pplyfun(1:length(season), function(i) {
        start <- substr(season[i],1,1) %>% as.integer()
        end <- substr(season[i],3,3) %>% as.integer()
        (start:end) + init.month
    })
    if (several.ok) out <- list(out)
    return(out)
}

#' @title Define number of members
#' @description Ascertain the number of members of the dataset from the parameter file
#' @param model model string, as read from the parameters file
#' @param model.type Model type. Accepted values are \code{"hindcast"}, \code{"forecast"} and \code{"ref"},
#' the latter yielding \code{NULL} (i.e., no members)
#' @param combination.method Combination method as inficated in the parameter file
#' @keywords internal
#' @return Integer, number of members

defineNmembers <- function(model, model.type, combination) {
    fcst.info <- loadTemplateFile("models")
    ref <- fcst.info[grep(model, fcst.info$qa4seas_code, fixed = TRUE), ]
    n <- if (model.type == "hindcast") {
        ref$HindcastEnsembleSize
    } else if (model.type == "forecast") {
        ref$ForecastEnsembleSize
    } else {
        NULL
    }
    if (!is.null(combination)) {
        if (combination == "multmodsimple") n <- 12
    }
    return(n)
}


#' @title Check model type
#' @description Checks the consistency between the model string and the type assigned (e.g., to avoid
#' defining ERA-Interim as a seasonal forecast).
#' @param model model string, as read from the parameters file
#' @param model.type Model type. Accepted values are \code{"hindcast"}, \code{"forecast"} and \code{"ref"}
#' @return Acts silently. Stops the execution if the check is not fulfilled
#' @keywords internal

checkModelType <- function(model, model.type) {
    if ((model == "ecmf-ei" | model == "ecmf-e5") && model.type != "ref") {
        stop(paste0("Incompatible \'model\' and \'model.type\' argument values (\'", model, "\' is of type \"ref\")"))
    }
    if ((model == "ecmf-system5" | model == "ecmf-system4" | model == "lfpw-system5" | model == "egrr-glosea5") && model.type == "ref") {
        stop(paste0("Incompatible \'model\' and \'model.type\' argument values (\'", model, "\' can't be of type \"ref\")"))
    }
}

#' @title Parse init dates
#' @description Interpret the initialization codes to readable dates (with obvious limitations, see Details)
#' @details The function will return the initialization time at the month level, assuming the first
#' of each month (note that proper initialization times should provide subdaily detail, but these won't be used by the QA4Seas prototype).
#' @param init Initialization time, as defined in the parameter file (\code{fcXXXX})
#' @param model.type Type of model. Accepted values are either \code{"forecast"} or \code{"hindcast"}
#' @param par.list Parameter list, as directly read from the parameter yaml file
#' @return A Date-class vector of initialization dates (this will have length one in the case of operative forecasts).
#' @keywords internal
#' @importFrom magrittr %>%


parseInit <- function(init, model.type, par.list) {
    stopifnot(is.list(par.list))
    mon <- substr(init, 3, 4) %>% as.integer()
    day <- substr(init, 5, 6) %>% as.integer()
    yrs <- if (model.type == "hindcast") {
        if (!"hindcast_period" %in% names(par.list)) stop("\'hindcast_period\' parameter not found")
        aux <- par.list$hindcast_period %>% gsub(pattern = "-", replacement = ":")
        parse(text = aux) %>% eval()
    } else {
        if (!"forecast_year" %in% names(par.list)) stop("\'forecast_year\' parameter not found")
        par.list$forecast_year %>% as.integer()
    }
    x <- paste(yrs, mon, day, sep = "-")
    as.Date(x, format = "%Y-%m-%d")
}

#' @title Calculate the number of days of a given month
#' @description Calculate the number of days of the current month (it takes into account leap years for februaries)
#' @param d A date (character) in format YYYY-MM-DD...
#' @return The number of days of the current month
#' @importFrom utils tail
#' @keywords internal

getNdays <- function(d) {
    as.difftime(tail((28:31)[which(!is.na(as.Date(paste0(substr(d, 1, 8), 28:31), '%Y-%m-%d')))], 1), units = "days")
}


#' @title Read template files
#' @description Read the template csv files in inst folder containing pre-defined metadata
#' @param file Which parameter file to read? Possible values are \code{"observations"},
#'  \code{"models"}, \code{"variables"} or \code{"products"}.
#' @return A \code{data.frame}
#' @importFrom utils read.table
#' @importFrom magrittr %>%
#' @keywords internal

loadTemplateFile <- function(file) {
    file <- match.arg(file, choices = c("observations", "models", "variables", "products"))
    filename <- switch(file,
                       "observations" = "template_ref_datasets.csv",
                       "models" = "template_fcst_datasets.csv",
                       "variables" = "template_variables.csv",
                       "products" = "template_products.csv")
    system.file(filename, package = "metaclipR.qa4seas") %>% read.table(header = TRUE, sep = ",",
                                                                        stringsAsFactors = FALSE,
                                                                        na.strings = "") %>% return()
}


#' @title Derive aggregation arg.list
#' @description Infer the aggregation parameters list given a variable and product type
#' @param product.type product type parameter string
#' @param var_code variable code string
#' @return An argument list, as required by \code{\link{[metaclipR]metaclipR.Aggregation}}
#' @keywords internal

setAggrArgList <- function(product.type, var_code) {
    arg.list <- list()
    ref.prod <- loadTemplateFile(file = "products")
    ref.var <- loadTemplateFile(file = "variables")
    ind.p <- grep(product.type, ref.prod$product_code, fixed = TRUE)
    ind.v <- grep(var_code, ref.var$var_code, fixed = TRUE)
    if (!is.na(ref.prod$aggr.lon[ind.p])) arg.list$aggr.lon$FUN <- ref.prod$aggr.lon[ind.p]
    if (!is.na(ref.prod$aggr.lat[ind.p])) arg.list$aggr.lat$FUN <- ref.prod$aggr.lat[ind.p]
    arg.list$aggr.d$FUN <- ref.var$aggr.d[ind.v]
    arg.list$aggr.m$FUN <- ref.var$aggr.m[ind.v]
    if (isTRUE(ref$aggr.y)) arg.list$aggr.y$FUN <- ref.var$aggr.y[ind.v]
    return(arg.list)
}

