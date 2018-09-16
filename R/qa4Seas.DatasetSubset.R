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


#' @title METACLIP definition of DatasetSubsets for QA4Seas prototype outcomes
#' @description Uses information from the parameter file to extract the DatasetSubset-class definition
#' @param package Software Package string
#' @param version Software package version string
#' @param graph Input graph (contains parent node).
#' @param var_code Character string with variable code
#' @param init Initialization time parameter. Passed by upper function
#' @param par.list Full parameter list
#' @param RefSpatialExtent A metaclipR object containing the spatial extent definition (HorizontalExtent-class)
#'  \code{"ENSO3"}, \code{"ENSO3dot4"}, \code{"ENSO4"}, and \code{"GlobalExtent"}
#' @param model Model designation, as indicated in the paraneter file
#' @param model.type Character string. Possible choices are \code{"ref"},
#' \code{"hindcast"} or \code{"forecast"}.
#' @param season Season vector (passed by upper function)
#' @param proj Projection. Unused, default to \code{"LatLon"}
#' @param resX Character. Spatial resolution of X coordinates. Unused, default to \code{"1_deg"}
#' @param resY Same as \code{resX}, for Y-coordinates
#' @param prod.info Product parameters, previously read
#' @param disable.command Logical flag, passed to the argument of the same name in \code{\link[metaclipR]{metaclipR.DatasetSubset}}
#' @importFrom igraph add_edges
#' @importFrom metaclipR my_add_vertices getNodeIndexbyName randomName my_union_graph metaclip.graph.Command setNodeName

# graph <- graph.hind
# model.type = "hindcast"
# proj = "LatLon"
# resX = "1_deg"
# resY = "1_deg"

qa4seas.DatasetSubset <- function(package, version, graph,
                                  var_code,
                                  init,
                                  par.list,
                                  RefSpatialExtent,
                                  model,
                                  model.type,
                                  season,
                                  prod.info,
                                  proj = "LatLon",
                                  resX = "1_deg",
                                  resY = "1_deg",
                                  disable.command = TRUE) {
    parent.node <- graph$parentnodename
    graph <- graph$graph
    if (class(graph) != "igraph") {
        stop("The input graph has not a valid format")
    }
    if (class(RefSpatialExtent$graph) != "igraph") stop("Invalid \'RefSpatialExtent\' structure")
    model <- match.arg(model,
                       choices = c("ecmf-ei", "ecmf-e5", "ecmf-system5",
                                   "ecmf-system4", "lfpw-system5", "egrr-glosea5"))
    model.type <- match.arg(model.type, choices = c("ref", "hindcast", "forecast"))
    checkModelType(model, model.type)
    # SubsetNode
    DatasetSubset.nodename <- paste0("DatasetSubset.", randomName())
    graph <- my_add_vertices(graph,
                             name = DatasetSubset.nodename,
                             label = "DatasetSubset",
                             className = "ds:DatasetSubset")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, parent.node),
                         getNodeIndexbyName(graph, DatasetSubset.nodename)),
                       label = paste0("ds:hadDatasetSubset"))
    # Variable  ---------------------
    # var.info <- read.table("ignore/qa4seas_prototype/template_variables.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
    var.info <- loadTemplateFile(file = "variables")
    var.meta <- var.info[grep(var_code, var.info$var_code, fixed = TRUE), ]
    shortname <- var.meta$shortName
    var.name <- var.meta$longname
    units <- var.meta$units
    vlevel <- var.meta$level
    var.nodename <- setNodeName(shortname, node.class = "Variable")
    graph <- my_add_vertices(graph,
                             name = var.nodename,
                             label = var.name,
                             className = "ds:Variable",
                             attr = list("ds:withUnits" = units,
                                         "ds:hasShortName" = shortname,
                                         "ds:hasVerticalLevel" = vlevel,
                                         "ds:hasHorizontalResX" = resX,
                                         "ds:hasHorizontalResY" = resY))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, var.nodename)),
                       label = "ds:hasVariable")
    # VariableStandardDefinition --------------------------------
    varstdef.nodename <- paste("VarStdDef", randomName(), sep = ".")
    graph <- my_add_vertices(graph,
                             name = varstdef.nodename,
                             label = "CF convention",
                             className = "ds:VariableStandardDefinition",
                             attr = list("ds:hasMainURL" = "http://cfconventions.org/standard-names.html",
                                         "ds:withVersionTag" = "v46"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, var.nodename),
                         getNodeIndexbyName(graph, varstdef.nodename)),
                       label = "ds:hasStandardDefinition")
    # TemporalResolution ---------------------
    timeres.nodename <- paste("TemporalResolution", randomName(), sep = ".")
    model.info <- loadTemplateFile("datasets")
    ref <- model.info[grep(model, model.info$qa4seas_code), ]
    time.step <- if (var.meta$atmos) {
        ref$AtmosTimeRes
    } else {
        ref$OceanTimeRes
    }
    time.step <- paste0("PT", time.step, "M")
    graph <- my_add_vertices(graph,
                             name = timeres.nodename,
                             label = time.step,
                             className = "ds:TemporalResolution",
                             attr = list("ds:hasTimeStep" = time.step))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, timeres.nodename)),
                       label = "ds:hasTemporalResolution")
    # HorizontalExtent -------------------------------------
    spatextent.nodename <- RefSpatialExtent$parentnodename
    spatextent.graph <- RefSpatialExtent$graph
    graph <- my_union_graph(graph, spatextent.graph)
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, spatextent.nodename)),
                       label = "ds:hasHorizontalExtent")
    # Realization ----------------------------------------------------
    if (model.type != "ref") {
        combination <- prod.info$combination_methods
        if (!is.null(combination)) {
            combination <- switch(combination,
                                  "multmodsimple" = "SimpleCombination",
                                  "multmodpdfaveq" = "PDFaverage",
                                  "multmodpdfavmd" = "PDFaverageEnsmean",
                                  "multmodpdfavrmse" = "PDFaverageRMSE",
                                  "multmodprobrps" = "ProbRPSCombination",
                                  "multmodprobeq" = "ProbCombination",
                                  "multimodresamp" = "ResampCombination"
            )
        }
        n.mem <- defineNmembers(model = model, model.type, combination = combination)
        if (!is.null(n.mem)) {
            member.node.names <- vector("list", n.mem)
            for (i in 1:n.mem) {
                mem <- paste0("Member_", i)
                member.node.names[[i]] <- paste("Realization", i, randomName(), sep = ".")
                graph <- my_add_vertices(graph,
                                         name = member.node.names[[i]],
                                         label = mem,
                                         className = "ds:Realization",
                                         attr = list("ds:hasMember" = mem))
                graph <- add_edges(graph,
                                   c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                                     getNodeIndexbyName(graph, member.node.names[[i]])),
                                   label = "ds:hasRealization")
            }
            # TemporalInstant (initialization) -----------------------------------------------
            initdates <- parseInit(init, "hindcast", par.list)
            for (i in 1:n.mem) {
                node.orig <- member.node.names[[i]]
                for (j in 1:length(initdates)) {
                    node.dest <- paste0(node.orig, "_init.", j)
                    init.date <- initdates[j]
                    graph <- my_add_vertices(graph,
                                             name = node.dest,
                                             label = init.date,
                                             className = "ds:TemporalInstant",
                                             attr = list("prov:generatedAtTime" = init.date))
                    graph <- add_edges(graph,
                                       c(getNodeIndexbyName(graph, node.orig),
                                         getNodeIndexbyName(graph, node.dest)),
                                       label = "ds:hasInitializationTime")
                }
            }
        }
    }
    # TemporalPeriod -----------------------------------------------
    filter.month <- as.list(season)
    # filter.month <- as.list(getSeason(output))
    names(filter.month) <- paste("ds:filterMonth", 1:length(filter.month), sep = ".")
    if (model.type != "ref") {
        startTime <- paste(substr(initdates[1],1,4), season[1], "01", sep = "-") %>% as.POSIXlt() %>%
            as.POSIXct() %>% format(format = "%Y-%m-%d")
        aux <- paste(substr(tail(initdates, 1),1,4), tail(season, 1), "01", sep = "-")
    } else {
        yr.range <- strsplit(par.list$hindcast_period, split = "-") %>% unlist()
        startTime <- paste("01", season[1], yr.range[1], sep = "-") %>% as.POSIXlt() %>%
            as.POSIXct() %>% format(format = "%Y-%m-%d")
        aux <- paste("01", tail(season,1), yr.range[2], sep = "-")
    }
    ndays <- getNdays(aux) %>% as.integer()
    endTime <- gsub("01$", ndays, aux) %>% as.POSIXlt() %>%
        as.POSIXct() %>% format(format = "%Y-%m-%d")
    timeper.nodename <- paste("TemporalPeriod", randomName(), sep = ".")
    seas.string <- substr(month.abb[unlist(filter.month)],1,1) %>% paste(collapse = "")
    lab <- paste(seas.string, paste(substr(startTime, 1, 4), substr(endTime, 1, 4), sep = "-"), sep = "/")
    graph <- my_add_vertices(graph,
                             name = timeper.nodename,
                             label = lab,
                             className = "ds:TemporalPeriod",
                             attr = c(list("prov:startedAtTime" = startTime,
                                           "prov:endedAtTime" = endTime),
                                      filter.month))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, DatasetSubset.nodename),
                         getNodeIndexbyName(graph, timeper.nodename)),
                       label = "ds:hasValidTemporalExtent")
    # ARGUMENT - COMMAND METADATA ----------------------------------------------
    if (!disable.command) {
        arg.list <- list("variables" = var_code,
                         "initialization_dates" = init,
                         "forecasting_systems" = model,
                         "origin_grid" = par.list$origin_grid,
                         "type" = prod.info$type,
                         "forecast_time_intervals" = prod.info$forecast_time_intervals,
                         "combination_methods" = prod.info$combination_methods)
        arg.list <- if (model.type == "forecast") {
            c(arg.list, "forecast_year" = par.list$forecast_year)
        } else {
            c(arg.list, "hindcast_period" = par.list$hindcast_period)
        }
        graph <- metaclip.graph.Command(graph,
                                        package,
                                        version,
                                        fun = "QA4Seas.py",
                                        arg.list = arg.list,
                                        origin.node.name = DatasetSubset.nodename)
    }
    return(list("graph" = graph, "parentnodename" = DatasetSubset.nodename))
}

