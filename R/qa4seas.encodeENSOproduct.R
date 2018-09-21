
#' @keywords internal
#' @importFrom metaclipR metaclipR.BiasCorrection metaclipR.Anomaly metaclipR.Validation metaclipR.ClimateIndex metaclipR.SpatialExtent

qa4seas.encodeENSOproduct <- function(pkg,
                                      v,
                                      par.list,
                                      prod.info,
                                      init,
                                      season) {
    index.code <- switch(prod.info$type,
                         "nino3.4" = "Nino3_4",
                         "nino3" = "Nino3",
                         "nino1.2" = "Nino1plus2",
                         "nino4" = "Nino4"
    )
    reg <- switch(prod.info$type,
                  "nino3.4" = "ENSO3dot4",
                  "nino3" = "ENSO3",
                  "nino1.2" = "ENSO3dot4",
                  "nino4" = "ENSO4")
    RefSpatialExtent <- metaclipR.SpatialExtent(region = reg)
    hind <- suppressMessages(startModelGraph(par.list = par.list,
                                             model.type = "hindcast",
                                             RefSpatialExtent = RefSpatialExtent,
                                             var_code = "sst",
                                             prod.info = prod.info,
                                             init = init,
                                             season = season))
    fcst <- suppressMessages(startModelGraph(par.list = par.list,
                                             model.type = "forecast",
                                             RefSpatialExtent = RefSpatialExtent,
                                             var_code = "sst",
                                             prod.info = prod.info,
                                             init = init,
                                             season = season))
    obs <- suppressMessages(startModelGraph(par.list = par.list,
                                            model.type = "ref",
                                            RefSpatialExtent = RefSpatialExtent,
                                            var_code = "sst",
                                            prod.info = prod.info,
                                            init = init,
                                            season = season))
    prev.season <- adjustPrevSeasonENSOplume(season = season)
    prev.obs <- suppressMessages(startModelGraph(par.list = par.list,
                                                 model.type = "ref",
                                                 RefSpatialExtent = RefSpatialExtent,
                                                 var_code = "sst",
                                                 prod.info = prod.info,
                                                 init = init,
                                                 season = prev.season))
    bc.fcst <- metaclipR.BiasCorrection(package = pkg, version = v,
                                        fun = "QA4Seas.py",
                                        arg.list = list(NULL),
                                        graph = fcst,
                                        TrainingGraph = hind,
                                        ReferenceGraph = obs,
                                        BC.method = "MVA", disable.command = TRUE)

    # graph2json(hind$graph, "/tmp/kk.json")

    # Anomaly calculation
    anom <- metaclipR.Anomaly(fun = "QA4Seas.py", graph = bc.fcst, package = pkg, version = v,
                              arg.list = list(NULL), referenceGraph = hind, disable.command = TRUE)
    nino <- metaclipR.ClimateIndex(graph = anom, package = pkg, version = v, index.code = index.code,
                                   new.time.res = "P1M", fun = "QA4Seas.py", arg.list = list(NULL), disable.command = TRUE)
    anom.obs <- metaclipR.Anomaly(fun = "QA4Seas.py", graph = obs, package = pkg, version = v, arg.list = list(NULL), disable.command = TRUE)
    anom.hind <- metaclipR.Anomaly(fun = "QA4Seas.py", graph = hind, package = pkg, version = v, arg.list = list(NULL), disable.command = TRUE)
    anom.prev.obs <- metaclipR.Anomaly(fun = "QA4Seas.py", graph = prev.obs, package = pkg, version = v, arg.list = list(NULL), disable.command = TRUE)
    # graph2json(anom$graph, "/tmp/anom.json")
    # Verification
    val <- metaclipR.Validation(package = pkg, version = v, fun = "QA4seas.py",
                                measure.name = "ACC", type = "verification",
                                QualityAspect = "Association",
                                PredictionGraph = anom.hind,
                                ReferenceGraph = anom.obs,
                                arg.list = list(NULL), disable.command = TRUE)
    mm <- ifelse(length(par.list$forecasting_systems) > 1, TRUE, FALSE)
    tit <- paste(prod.info$type, "Plume") %>% gsub(pattern = "nino", replacement = "ENSO ")
    gp <- qa4seas.EnsoPlume(multimodel = mm,
                            FcstMetadata = nino,
                            ObsMetadata = anom.prev.obs,
                            VerificationMetadata = val,
                            title = tit,
                            arg.list = c(list("variables" = "sst",
                                            "initialization_dates" = init,
                                            "forecasting_systems" = par.list$forecasting_systems,
                                            "origin_grid" = par.list$origin_grid,
                                            "type" = prod.info$type,
                                            "forecast_time_intervals" = prod.info$forecast_time_intervals,
                                            "combination_methods" = prod.info$combination_methods,
                                            "forecast_year" = par.list$forecast_year,
                                            "hindcast_period" = par.list$hindcast_period), prod.info))
    invisible(gp)
}
