##     qa4seas.EnsoPlume Construct a directed graph for ENSO plume plots
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

#' @title Directed metadata graph construction for ENSO plume graphical products
#' @description Build a directed metadata graph describing a ENSO plume plot from the QA4Seas prototype
#' @param package package
#' @param version version
#' @param fun function name.
#' @param multimodel Logical. Is the forecast done with a multimodel ensemble dataset?. Default
#' to \code{FALSE} (This is needed to ascertain the subtypes of ForecastSystemUncertainty communicated
#' by the product, i.e. FSi/FSm).
#' @param arg.list Argument list
#' @param FcstMetadata The metadata structure containing the step describing the forecast data provenance
#' @param ObsMetadata The metadata structure containing the step describing the observation data provenance
#' @param VerificationMetadata The metadata structure containing the step describing the verification provenance.
#' Only needed when EQC info (ACC) is added to the plume
#' @param title Character string character with the plot title
#' @param multimodel Logical. Is it a multi-model product?
#' @family graphical.products
#' @keywords internal
#' @importFrom igraph make_empty_graph add_vertices add_edges
#' @importFrom metaclipR my_union_graph
#' @importFrom magrittr %>%


qa4seas.EnsoPlume <- function(package = "QA4Seas-prototype",
                              version = "1.0.1",
                              fun = "QA4seas.py",
                              multimodel = FALSE,
                              arg.list = NULL,
                              FcstMetadata,
                              ObsMetadata = NULL,
                              VerificationMetadata = NULL,
                              title = "ENSO Plume") {
    if (is.null(FcstMetadata$graph)) stop("The FcstMetadata input is required")
    graph <- FcstMetadata$graph
    if (class(graph) != "igraph") stop("Invalid input graph (not an 'igraph-class' object)")
    withInput <- FcstMetadata$parentnodename
    if (!is.null(ObsMetadata)) {
        withObsGraph <- ObsMetadata$graph
        withObsGraphNodeName <- ObsMetadata$parentnodename
    }
    if (!is.null(VerificationMetadata)) {
        withVerificationGraph <- VerificationMetadata$graph
        withVerificationGraphNodeName <- VerificationMetadata$parentnodename
    }
    chart.nodename <- paste("Chart", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = chart.nodename,
                          label = "Chart",
                          className = "go:Chart",
                          attr = list("go:hasProductType" = "Seasonal Forecast + EQC Product")
    )
    # X Axis (anomalies) -----------------------------------
    timeaxis.nodename <- paste("TimeChartAxis", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = timeaxis.nodename,
                          label = "ChartTimeAxis",
                          className = "go:TimeChartAxis",
                          attr = list("go:hasChartAxisUnits" = "months",
                                      "go:hasChartAxisType" = "X",
                                      "go:hasChartAxisScale" = "linear",
                                      "go:hasChartAxisMinValue" = "undefined",
                                      "go:hasChartAxisMaxValue" = "undefined"))
    # Y axis (months) -------------------------------------
    anomaxis.nodename <- paste("MagnitudeChartAxis", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = anomaxis.nodename,
                          label = "AnomalyAxis",
                          className = "go:MagnitudeChartAxis",
                          attr = list("go:hasChartAxisUnits" = "degC anomaly",
                                      "go:hasChartAxisType" = "Y",
                                      "go:hasChartAxisScale" = "linear",
                                      "go:hasChartAxisMinValue" = "undefined",
                                      "go:hasChartAxisMaxValue" = "undefined"))
    # LAYER1 Forecast spaghettis -------------------------
    chartlinesfcst.nodename <- paste("ChartLines", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = chartlinesfcst.nodename,
                          label = "ChartLinesForecast",
                          className = "go:ChartLines",
                          attr = list("go:hasLayerDescription" = "Spaghetti forecast time series",
                                      "go:hasChartLayerMinXValue" = "undefined",
                                      "go:hasChartLayerMaxXValue" = "undefined",
                                      "go:hasChartLayerMinYValue" = "undefined",
                                      "go:hasChartLayerMaxYValue" = "undefined"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chart.nodename),
                         getNodeIndexbyName(graph, chartlinesfcst.nodename)),
                       label = "go:hasChartLayer")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, withInput),
                         getNodeIndexbyName(graph, chartlinesfcst.nodename)),
                       label = "go:hadGraphicalRepresentation")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chartlinesfcst.nodename),
                         getNodeIndexbyName(graph, timeaxis.nodename)),
                       label = "go:hasChartAxis")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chartlinesfcst.nodename),
                         getNodeIndexbyName(graph, anomaxis.nodename)),
                       label = "go:hasChartAxis")
    # Add FS Uncertainty -------------------------------
    uncert.nodename <- paste("Uncertainty", randomName(), sep = ".")
    fsutype <- ifelse(multimodel, "FSi/FSm", "FSi")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = uncert.nodename,
                          label = "Uncertainty",
                          className = "go:ForecastSystemUncertainty",
                          attr = list("go:hasForecastSystemUncertaintyType" = fsutype))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chartlinesfcst.nodename),
                         getNodeIndexbyName(graph, uncert.nodename)),
                       label = "go:hasUncertainty")
    # LAYER2 Observed line -------------------------------
    chartlinesobs.nodename <- paste("ChartLines", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = chartlinesobs.nodename,
                          label = "ChartLinesObs",
                          className = "go:ChartLines",
                          attr = list("go:hasLayerDescription" = "Observations time series",
                                      "go:hasChartLayerMinXValue" = "undefined",
                                      "go:hasChartLayerMaxXValue" = "undefined",
                                      "go:hasChartLayerMinYValue" = "undefined",
                                      "go:hasChartLayerMaxYValue" = "undefined"))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chart.nodename),
                         getNodeIndexbyName(graph, chartlinesobs.nodename)),
                       label = "go:hasChartLayer")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chartlinesobs.nodename),
                         getNodeIndexbyName(graph, timeaxis.nodename)),
                       label = "go:hasChartAxis")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chartlinesobs.nodename),
                         getNodeIndexbyName(graph, anomaxis.nodename)),
                       label = "go:hasChartAxis")
    if (!is.null(withObsGraph)) {
        if (is.null(withObsGraphNodeName)) stop("'withObsGraphNodeName' is required")
        # Linking the product graph with the observations graph
        graph <- my_union_graph(graph, withObsGraph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, withObsGraphNodeName),
                             getNodeIndexbyName(graph, chartlinesobs.nodename)),
                           label = "go:hadGraphicalRepresentation")
    }
    # Additional layers
    ## Horizontal 0 line ------------------------------
    chartlines0.nodename <- paste("ChartLines", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = chartlines0.nodename,
                          label = "0-ordinate",
                          className = "go:ChartLines",
                          attr = list("go:hasLayerDescription" = "0-anomaly reference line",
                                      "go:hasChartLayerMinXValue" = "undefined",
                                      "go:hasChartLayerMaxXValue" = "undefined",
                                      "go:hasChartLayerMinYValue" = as.character(0),
                                      "go:hasChartLayerMaxYValue" = as.character(0)))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chart.nodename),
                         getNodeIndexbyName(graph, chartlines0.nodename)),
                       label = "go:hasChartLayer")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chartlines0.nodename),
                         getNodeIndexbyName(graph, timeaxis.nodename)),
                       label = "go:hasChartAxis")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chartlines0.nodename),
                         getNodeIndexbyName(graph, anomaxis.nodename)),
                       label = "go:hasChartAxis")
    # Textual annotations ---------------------
    # title -----------
    textannot.nodename <- paste("TextAnnotation", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = textannot.nodename,
                          label = "Chart Title",
                          className = "go:TextLayer",
                          attr = list("go:hasTextAnnotationType" = title))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chart.nodename),
                         getNodeIndexbyName(graph, textannot.nodename)),
                       label = "go:hasTextLayer")
    # EQC info
    textannot.nodename <- paste("TextAnnotation", randomName(), sep = ".")
    descr <- "EQC info: Anomaly Correlation Coefficient with confidence interval"
    graph <- add_vertices(graph,
                          nv = 1,
                          name = textannot.nodename,
                          label = "EQC text info",
                          className = "go:TextLayer",
                          attr = list("go:hasTextAnnotationType" = descr))
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, chart.nodename),
                         getNodeIndexbyName(graph, textannot.nodename)),
                       label = "go:hasTextLayer")
    if (!is.null(withVerificationGraph)) {
        if (is.null(withVerificationGraphNodeName)) stop("'withVerificationGraphNodeName' is required")
        # Linking the EQC text layer with the verification graph
        graph <- my_union_graph(graph, withVerificationGraph)
        graph <- add_edges(graph,
                           c(getNodeIndexbyName(graph, withVerificationGraphNodeName),
                             getNodeIndexbyName(graph, textannot.nodename)),
                           label = "go:hadGraphicalRepresentation")
    }
    # Add EQC uncertainty ------------------------
    uncert.nodename <- paste("Uncertainty", randomName(), sep = ".")
    graph <- add_vertices(graph,
                          nv = 1,
                          name = uncert.nodename,
                          label = "Sampling uncertainty",
                          className = "go:SamplingUncertainty")
    graph <- add_edges(graph,
                       c(getNodeIndexbyName(graph, textannot.nodename),
                         getNodeIndexbyName(graph, uncert.nodename)),
                       label = "go:hasUncertainty")

    # Package/Command/Argument metadata ---------------------------------------
    graph <- metaclip.graph.Command(graph, package, version, fun, arg.list,
                                    origin.node.name = chart.nodename)
    return(list("graph" = graph, "parentnodename" = chart.nodename))
}


