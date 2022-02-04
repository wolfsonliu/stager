## -*- coding: utf-8 -*-
##' @name factory.stage.variables
##'
##' @title Calculating of stage variables using \code{optim()}
##'
##' @description \code{factory.stage.variables} is a factory function
##'     that returns a function used for the calculation of variables
##'     for stage. The output function should be applied to
##'     \code{optim()} funtion.
##'
##' @param day int vector, days of observation, should be in ascending
##'     order.
##' @param daycount int vector, counts of birds observed for each day,
##'     should be coordinated with day vector.
##' @param population int or unset for prediction, the total number of
##'     all the birds, including migrants and residents.
##' @param immigrate.mean numeric or unset for prediction, the mean
##'     day of immigration.
##' @param immigrate.sd numeric or unset for prediction, the standard
##'     diviation of immigration days.
##' @param emigrate.mean numeric or unset for prediction, the mean day
##'     of emigration.
##' @param emigrate.sd numeric or unset for prediciton, the standard
##'     diviation of emigration days.
##' @param resident numeric or unset for prediction, the number of
##'     resident birds.
##'
##' @return \code{factory.stage.variables(day, daycount, population,
##'     immigrate.mean, immigrate.sd, emigrate.mean, emigrate.sd)}
##'     returns the function used for prediction with \code{optim}.
##'
##' @details The \code{factory.stage.variables} should be assigned
##'     with 'day' and 'daycount' parameters. Other parameters are
##'     optional. You should only assign the known parameters to the
##'     \code{factory.stage.variables} function. And other unknown
##'     parameters should be calculated with \code{optim()} function,
##'     with min and max range provided.
##'
##' @seealso \code{optim()} for calculation using the output function of
##'     \code{factory.stage.variables()}, \code{calculate.stage.variables()}
##'     for easier calculation of stage variables.
##'
##' @examples
##' data <- data.frame(
##'     day=c(11, 16, 20, 22, 24, 34, 39, 40, 47, 60),
##'     daycount=c(10, 65, 60, 45, 66, 52, 35, 50, 27, 0)
##' )
##'
##' aimfunc <- factory.stage.variables(
##'     day=data$day, daycount=data$daycount, population=85
##' )
##'
##' result <- optim(
##'     par=c(
##'         'immigrate.mean'=15, 'emigrate.mean'=40,
##'         'immigrate.sd'=5, 'emigrate.sd'=15),
##'     fn=aimfunc,
##'     method="L-BFGS-B",
##'     lower=c(
##'         'immigrate.mean'=10, 'emigrate.mean'=20,
##'         'immigrate.sd'=4, 'emigrate.sd'=10
##'     ),
##'     upper=c(
##'         'immigrate.mean'=25, 'emigrate.mean'=50,
##'         'immigrate.sd'=10, 'emigrate.sd'=30
##'     )
##' )
##'
##' @importFrom stats optim
##' @export
factory.stage.variables <- function(day,
                                    daycount,
                                    population,
                                    resident,
                                    immigrate.mean,
                                    immigrate.sd,
                                    emigrate.mean,
                                    emigrate.sd)
{
    known.variables <- as.list(match.call())[-1]
    optimize.func <- function(vars) {
        variables <- c(
            known.variables,
            vars[!(names(vars) %in% known.variables)]
        )
        stage.population <- stage.number
        formals(stage.population) <- variables
        model.count <- stage.population()
        sum((model.count - daycount)**2) / length(variables[['day']])
    }
    optimize.func
}
##'
##' @name calculate.stage.variables
##'
##' @title Calculating the stage variables
##'
##' @description A wrapper function of \code{optim()} for the easier
##'     calculation of stage variables using the output function from
##'     \code{factory.stage.variables()}.
##'
##' @param day int vector, days of observation, should be in ascending
##'     order.
##' @param daycount int vector, counts of birds observed for each day,
##'     should be coordinated with day vector.
##' @param population int or range vector of \code{c(min, max)} for
##'     prediction, the total number of all the birds, including
##'     migrants and residents.
##' @param immigrate.mean numeric or range vector of \code{c(min,
##'     max)} for prediction, the mean day of immigration.
##' @param immigrate.sd numeric or range vector of \code{c(min, max)}
##'     for prediction, the standard diviation of immigration days.
##' @param emigrate.mean numeric or range vector of \code{c(min, max)}
##'     for prediction, the mean day of emigration.
##' @param emigrate.sd numeric or range vector of \code{c(min, max)}
##'     for prediciton, the standard diviation of emigration days.
##' @param resident numeric or range vector of \code{c(min, max)} for
##'     prediction, the number of resident birds.
##'
##' @return \code{calculate.stage.variables(day, daycount, population,
##'     immigrate.mean, immigrate.sd, emigrate.mean, emigrate.sd)}
##'     returns a list of output of calculated variables for the stage
##'     of the data inputed.
##'
##' @seealso \code{factory.stage.variables()} for a lower level function to calculate,
##'     \code{optim()} for calculation using the output function of
##'     \code{factory.stage.variables()}.
##'
##' @examples
##' data <- data.frame(
##'     day=c(11, 16, 20, 22, 24, 34, 39, 40, 47, 60),
##'     daycount=c(10, 65, 60, 45, 66, 52, 35, 50, 27, 0)
##' )
##'
##' result <- calculate.stage.variables(
##'     day=data$day,
##'     daycount=data$daycount,
##'     population=85,
##'     resident = 0,
##'     immigrate.mean=c(10, 20),
##'     emigrate.mean=c(20, 50),
##'     immigrate.sd=c(4, 10),
##'     emigrate.sd=c(10, 30)
##' )
##'
##' @export
calculate.stage.variables <- function(day,
                                      daycount,
                                      population,
                                      resident,
                                      immigrate.mean,
                                      immigrate.sd,
                                      emigrate.mean,
                                      emigrate.sd)
{
    all.variables <- lapply(
        as.list(match.call())[-1],
        eval
    )
    known.variables <- list()
    known.variables$day <- all.variables$day
    known.variables$daycount <- all.variables$daycount
    predict.variables <- list()
    predict.variables$lower <- list()
    predict.variables$upper <- list()
    for (x in c('population',
                'immigrate.mean', 'immigrate.sd',
                'emigrate.mean', 'emigrate.sd',
                'resident')) {
        if (length(all.variables[[x]]) == 1) {
            known.variables[[x]] <- all.variables[[x]]
        } else if (length(all.variables[[x]]) == 2) {
            predict.variables$lower[[x]] <- all.variables[[x]][1]
            predict.variables$upper[[x]] <- all.variables[[x]][2]
        }
    }
    aimfunc <- do.call('factory.stage.variables', known.variables)

    result <- optim(
        par=unlist(predict.variables$lower),
        fn=aimfunc,
        method="L-BFGS-B",
        lower=unlist(predict.variables$lower),
        upper=unlist(predict.variables$upper)
    )

    output.list <- list()
    for (x in names(known.variables)) {
        output.list[[x]] <- known.variables[[x]]
    }
    for (x in names(result$par)) {
        output.list[[x]] <- result$par[x]
        if (x == 'population') {
            output.list[['population CI(0.95)']] <- qchisq(
                c(0.025, 0.975),
                df = result$par[['population']] * 2
            ) / 2
        }
    }
    output.list[['mse']] <- result$value
    class(output.list) <- c('stage', 'list')
    output.list
}
