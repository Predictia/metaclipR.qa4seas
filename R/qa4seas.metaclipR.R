#' @title QA4Seas prototype metaclip launcher
#' @description Given a prototype input yaml file, parses the parameters to yield json files with metadata description for
#'  the outcomes generated
#' @param parameter.file Input yaml parameter file
#' @param pkg Software package. Default to \code{"QA4Seas-prototype"}, which has an individual description in the datasource ontology.
#' @param v Software package version character string. Default to \code{"1.0.1"}
#' @param out.dir Full path to the output directory the output json files are to be stored
#' @importFrom metaclipR graph2json
#' @export

qa4seas.metaclipR <- function(parameter.file,
                              pkg = "QA4Seas-prototype",
                              v = "1.0.1",
                              out.dir) {
    par.list <- yaml::read_yaml(parameter.file)
    variables <- par.list$variables
    inits <- par.list$initialization_dates
    nproducts <- length(par.list$products)
    ref.string <- par.list$reference
    grid.string <- par.list$origin_grid
    period.string <- par.list$hindcast_period
    ## Iterate over variables
    for (i in 1:length(variables)) {
        var_code <- variables[i]
        ## Iterate over inits
        for (j in 1:length(inits)) {
            init <- inits[j]
            init.string <- paste0("init", substr(init,3,4))
            ## Iterate over products
            for (k in 1:length(nproducts)) {
                prod.info <- par.list$products[[k]]
                # Iterate over forecasting systems and apply combination if in use
                fsys <- par.list$forecasting_systems %>% as.list()
                if (!is.null(prod.info$combination_methods)) {
                    fsys <- par.list$forecasting_systems %>% as.list()
                    if ((length(fsys) > 1)) {
                        fsys[[length(fsys) + 1]] <- sapply(par.list$forecasting_systems, "c", USE.NAMES = FALSE)
                    }
                }
                for (h in 1:length(fsys)) {
                    par.list$forecasting_systems <- fsys[[h]]
                    model.string <- paste(par.list$forecasting_systems, collapse = "-")
                    several.ok <- ifelse(grepl("^nino", prod.info$type), TRUE, FALSE)
                    seasons <- parseSeason(init = init, season = prod.info$forecast_time_intervals, several.ok = several.ok)
                    anom.string <- prod.info$anomaly_types
                    prob.string <- NULL
                    if (!is.null(prod.info$probabilistic_scores)) {
                        if (!is.null(prod.info$quantile_thresholds)) {
                            prob.string <- paste("prob", prod.info$quantile_thresholds, sep = "-")
                        }
                    }
                    ## Iterate over seasons
                    for (l in 1:length(seasons)) {
                        season <- seasons[[l]]
                        # File naming
                        seastring <- paste(substr(month.abb, 1,1)[unlist(season)], collapse = "")
                        if (!is.null(prod.info$combination_methods) && (h == length(fsys))) {
                            if (is.null(anom.string)) {
                                of <- paste0(seastring, "-", period.string, "-", ref.string, "_", model.string, "_",
                                             prod.info$combination_methods, "_", var_code, "_", grid.string, "_", init.string)
                            } else {
                                of <- paste0(seastring, "-", period.string, "-", ref.string, "_", model.string, "_",
                                             prod.info$combination_methods, "_", anom.string, "_", var_code, "_", grid.string, "_", init.string)
                            }
                        } else {
                            if (is.null(anom.string)) {
                                of <- paste0(seastring, "-", period.string, "-", ref.string, "_", model.string,
                                             "_", var_code, "_", grid.string, "_", init.string)
                            } else {
                                of <- paste0(seastring, "-", period.string, "-", ref.string, "_", model.string,
                                             "_", anom.string,"_",var_code, "_", grid.string, "_", init.string)
                            }
                        }
                        if (!is.null(prob.string)) {
                            of <- gsub(pattern = model.string, x = of, replacement = paste0(model.string, "_", prob.string))
                        }
                        if (grepl("^nino.*", prod.info$type)) {
                            of <- paste(prod.info$type, of, sep = "-")
                            gp <- qa4seas.encodeENSOproduct(pkg = pkg, v = v, par.list = par.list,
                                                            prod.info = prod.info, init = init, season = season)
                            graph2json(graph = gp$graph, output.file = paste0(out.dir, "/", of, ".json"))
                        } else if (grepl("^map", prod.info$type)) {
                            scores <- c(prod.info$probabilistic_scores, prod.info$deterministic_scores)
                            for (m in 1:length(scores)) {
                                of <- paste(scores[m], of, sep = "-")
                                gp <- qa4seas.encodeSkillMapProduct(pkg = pkg, v = v, par.list = par.list,
                                                                    prod.info = prod.info,
                                                                    init = init, season = season,
                                                                    var_code = var_code,
                                                                    score = scores[m])
                                graph2json(graph = gp$graph, output.file = paste0(out.dir, "/", of, ".json"))
                            }
                        } else if (grepl("^climagram", prod.info$type)) {
                            stop(prod.info$type, "product METACLIP description is currently not available")
                        } else {
                            stop(prod.info$type, "product METACLIP description is currently not available")
                        }
                    }
                }
            }
        }
    }
}