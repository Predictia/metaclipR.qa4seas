#' @importFrom metaclipR metaclipR.Interpolation metaclipR.Validation metaclipR.SkillMap metaclipR.SpatialExtent

qa4seas.encodeSkillMapProduct <- function(pkg, v, par.list, prod.info, init, season, score, var_code) {
    RefSpatialExtent <- metaclipR.SpatialExtent(region = "GlobalExtent")
    hind <- suppressMessages(startModelGraph(par.list = par.list,
                                             model.type = "hindcast",
                                             RefSpatialExtent = RefSpatialExtent,
                                             var_code = var_code,
                                             prod.info = prod.info,
                                             init = init,
                                             season = season))
    obs <- suppressMessages(startModelGraph(par.list = par.list,
                                            model.type = "ref",
                                            RefSpatialExtent = RefSpatialExtent,
                                            var_code = var_code,
                                            prod.info = prod.info,
                                            init = init,
                                            season = season))
    obs <- metaclipR.Interpolation(graph = obs, RefSpatialExtent = RefSpatialExtent,
                                   disable.command = TRUE, InterpolationMethod = "bilinear")
    qa <- switch(score,
                 "catRPS" = "Accuracy",
                 "catAUC" = "Discrimination",
                 "catBS" = "Accuracy",
                 "EnsCor" = "Association",
                 "Ens2AFC" = "Discrimination",
                 "EnsCrpss" = "Accuracy")
    mn <- switch(score,
                 "catRPS" = "RankedProbabilityScore",
                 "catAUC" = "AUC",
                 "catBS" = "BrierScore",
                 "EnsCor" = "EnsembleCorrelation",
                 "Ens2AFC" = "GeneralizedDiscriminationScore",
                 "EnsCrpss" = "ContRankedProbabilitySkillScore")
    val <- metaclipR.Validation(QualityAspect = qa,
                                measure.name = mn,
                                arg.list = list(NULL),
                                type = "verification",
                                PredictionGraph = hind,
                                ReferenceGraph = obs,
                                disable.command = TRUE)
    gp <- metaclipR.SkillMap(graph = val, fun = "QA4seas.py", package = pkg, version = v,
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