## -*- coding: utf-8 -*-
##' @name stage.number
##'
##' @title Calulating of staging bird numbers by stage variables.
##'
##' @description Function for calculation of staging bird numbers
##'     using normal distributions of immigration and emigration.
##'
##' @param day int, the day from the start of migration.
##' @param population int, the total number of all the birds,
##'     including migrants and residents.
##' @param immigrate.mean numeric, the mean day of immigration.
##' @param immigrate.sd numeric, the standard diviation of immigration
##'     days.
##' @param emigrate.mean numeric, the mean day of emigration.
##' @param emigrate.sd numeric, the standard diviation of emigration
##'     days.
##' @param resident numeric, the number of
##'     resident birds.
##'
##' @return \code{stage.number(day, population, immigrate.mean,
##'     immigrate.sd, emigrate.mean, emigrate.sd)} returns the number
##'     of the staging birds of gaven days.
##'
##' @examples
##' stage.number(
##'     day=10, population=5000,
##'     immigrate.mean=15, immigrate.sd=8,
##'     emigrate.mean=45, emigrate.sd=20
##' )
##'
##' @export
stage.number <- function(day,
                         population,
                         resident,
                         immigrate.mean,
                         immigrate.sd,
                         emigrate.mean,
                         emigrate.sd) {
    migrant <- population - resident
    migrate.number(
        day=day,
        population=migrant,
        migrate.mean=immigrate.mean,
        migrate.sd=immigrate.sd
    ) -
        migrate.number(
            day=day,
            population=migrant,
            migrate.mean=emigrate.mean,
            migrate.sd=emigrate.sd
        ) +
        resident -
        migrate.number(
            day=day,
            population=resident,
            migrate.mean=emigrate.mean,
            migrate.sd=emigrate.sd
        )
}
