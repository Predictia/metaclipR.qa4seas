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
        sea <- (start:end) + init.month
        sea[which(sea > 12)] <- sea[which(sea > 12)] - 12
        return(sea)
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
    fcst.info <- loadTemplateFile(file = "datasets")
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
#' @param file Which parameter file to read? Possible values are
#'  \code{"datasets"}, \code{"variables"} or \code{"products"}.
#' @return A \code{data.frame}
#' @importFrom utils read.table
#' @importFrom magrittr %>%
#' @keywords internal

loadTemplateFile <- function(file) {
    file <- match.arg(file, choices = c("datasets", "variables", "products"))
    filename <- switch(file,
                       "datasets" = "template_datasets.csv",
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
#' @return An argument list, as required by \code{\link[metaclipR]{metaclipR.Aggregation}}
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
    if (isTRUE(ref.prod$aggr.y[ind.p])) arg.list$aggr.y$FUN <- ref.var$aggr.y[ind.v]
    return(arg.list)
}




#' @title Start a QA4Seas dataset graph
#' @description Start a QA4Seas dataset graph fiev a dataset (possibly a multimodel)
#' @param pkg pkg
#' @param v version
#' @param fun function
#' @param par.list Full list of parameters
#' @param model.type Character string specifying the type of model.
#' @param RefSpatialExtent Ref spatial extent
#' @param var_code individual var code
#' @param prod.info individual product info list
#' @param init individual init
#' @param season individual season
#' @param disable.command. Default to \code{TRUE}. Passed to \code{\link{qa4seas.DatasetSubset}}
#' @return A metaclipR structure. The terminal node belong either to the specific subclass of Dataset-class
#' for model lists of length one, or ds:Ensemble-class, in case several models are included in
#'  the \code{forecasting_system} parameter.
#' @details The function internally handles the recursive listing in case of multimodel ensembles.
#' The specific inputs come from the ouer wrapping function
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom metaclipR metaclipR.Aggregation metaclipR.Ensemble metaclipR.Dataset

# model.par = models
# model.type = "hindcast"

startModelGraph <- function(pkg = "QA4Seas-prototype", v = "1.0.1",
                            fun = "QA4Seas.py",
                            par.list, model.type, RefSpatialExtent,
                            var_code, prod.info, init, season,
                            disable.command = TRUE) {
    model.type <- match.arg(model.type, choices = c("ref", "hindcast", "forecast"))
    model.par <- if (model.type == "ref") {
        par.list$reference
    } else {
        par.list$forecasting_systems
    }
    fcst.info <- loadTemplateFile(file = "datasets")
    ds.class <- switch(model.type,
                       "hindcast" = "SeasonalHindcast",
                       "forecast" = "SeasonalOperationalForecast",
                       "ref" = "Reanalysis"
    )
    h.list <- lapply(1:length(model.par), function(x) {
        model <- model.par[x]
        ref <- fcst.info[grep(model, fcst.info$qa4seas_code, fixed = TRUE),]
        aux <- ifelse(ds.class == "Reanalysis", ref$Dataset_name, model)
        g <- metaclipR::metaclipR.Dataset(Dataset.name = aux,
                                          GCM = ref$SeasonalForecastingSystem,
                                          Dataset.subclass = ds.class,
                                          DataProvider = ref$DataProvider,
                                          ModellingCenter = ref$ModellingCenter,
                                          Project = ref$Project)
        ## Define datasetSubsets
        g <- qa4seas.DatasetSubset(package = pkg, version = v,
                                   graph = g, var_code,
                                   init,
                                   par.list,
                                   RefSpatialExtent,
                                   model,
                                   model.type = model.type,
                                   season,
                                   prod.info,
                                   disable.command = disable.command)
        ## Define aggregations
        g <- metaclipR.Aggregation(package = pkg, version = v,
                                   graph = g,
                                   fun = fun,
                                   arg.list = setAggrArgList(prod.info$type, var_code),
                                   use.arg.list = FALSE,
                                   disable.command = disable.command)

    })
    out <- if (length(h.list) > 1) {
        metaclipR.Ensemble(package = pkg,
                           version = v,
                           graph.list = h.list,
                           fun = fun,
                           combination.method = parseCombinationMethod(prod.info),
                           disable.command = disable.command)
    } else {
        h.list[[1]]
    }
    invisible(out)
}


#' @title Infer season of the previous observed time series
#' @description Infer the season of the previous observed time series in ENSO plume plots
#' @param season Season of the observed reference used as reference for ACC and anomaly calculation
#' @param n.prev.months Number of previous months used before initialization. Default to 4, as usual in C3S plumes
#' @return An integer vector, with the season definition
#' @details interna helper used for metaclipR.ENSOplume in the QA4Seas prototype
#' @keywords internal

adjustPrevSeasonENSOplume <- function(season, n.prev.months = 4) {
    ref <- 1:12
    ini <- season[1] - 1
    if (ini == 0) ini <- 12
    end.ref <- match(ini, ref)
    start.month <- ref[end.ref] - n.prev.months
    if (start.month <= 0) start.month <- 12 + start.month
    start.ref <- match(start.month, ref)
    new.season <- if (start.ref < end.ref) {
        ref[start.ref:end.ref]
    } else {
        c(start.ref,12,1:end.ref)
    }
    return(new.season)
}

#' @keywords internal

parseCombinationMethod <- function(prod.info) {
    if (!is.null(prod.info$combination_methods)) {
        switch(prod.info$combination_methods,
               "multmodsimple" = "SimpleCombination",
               "multmodresamp" = "ResampCombination",
               "multmodprobrps" = "ProbRPSCombination",
               "multmodprobeq" = "ProbCombination",
               "multmodpdfavrmse" = "PDFaverageRMSE",
               "multmodpdfavmd" = "PDFaverageEnsmean" ,
               "multmodpdfaveq" = "PDFaverage")
    } else {
        NULL
    }
}
