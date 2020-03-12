## -*- coding: utf-8 -*-
##' @name migrate.proportion
##' @aliases migrate.number
##'
##' @rdname migrate
##'
##' @title Calculating of stage parameters.
##'
##' @description \code{migrate.proportion()} calculates the
##'     immigration or emmigration proportion of whole population
##'     using normal distribution.
##'
##' @param day int, the day from the start of migration.
##' @param migrate.mean numeric, the mean day of immigration or
##'     emigration.
##' @param migrate.sd numeric, the standard diviation of immigration
##'     days or emigration days.
##'
##' @return \code{migrate.proportion(day, migrate.mean, migrate.sd)}
##'     returns the proportion of the migrated birds (immigration or
##'     emigration).
##'
##' @examples
##' migrate.proportion(10, 15, 8)
##'
##' @importFrom stats pnorm
##' @export
migrate.proportion <- function(day, migrate.mean, migrate.sd) {
    pnorm(day, mean=migrate.mean, sd=migrate.sd)
}
##'
##' @rdname migrate
##'
##' @description \code{migrate.number()} calculates the immigration or
##'     emmigration numbers. \code{migrate.number()} returns the
##'     proportion result from \code{migrate.proportion()} times the
##'     whole population number.
##'
##' @param day int, the day from the start of migration.
##' @param population int, the total number of all the migrated birds.
##' @param migrate.mean numeric, the mean day of immigration day or
##'     emigration.
##' @param migrate.sd numeric, the standard diviation of immigration
##'     days or emigration days.
##'
##' @return \code{migrate.number(day, population, migrate.mean,
##'     migrate.sd)} returns the number of the migrated birds
##'     (immigration or emigration).
##'
##' @seealso [calculate.stage.variables()] for the estimation of
##'     important stage variables.
##'
##' @examples
##' migrate.number(10, 5000, 15, 8)
##'
##' @export
migrate.number <- function(day, population, migrate.mean, migrate.sd) {
    migrate.proportion(
        day, migrate.mean=migrate.mean, migrate.sd=migrate.sd
    ) * population
}
