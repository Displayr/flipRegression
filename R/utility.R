# Returns a logical whether the model has been weighted or not
isWeightedModel <- function(model) {
    # Examine the actual model if a Regression output
    if (inherits(model, "Regression"))
        return(Recall(model[["original"]]))
    # If model created using survey, then it is weighted
    if (issvyglm(model) || inherits(model, "svyolr"))
        return(TRUE)
    # Check if the (weights) variable exists if created using glm.nb
    if (inherits(model, "negbin") && !is.null(model[["model"]][["(weights)"]]))
        return(TRUE)
    # Check if there are any non-unit weights in multinom output
    inherits(model, "multinom") && any(model[["weights"]] != 1L)
}

# Takes a regression model and returns the type of model
#' @importFrom stats family
getModelType <- function(model) {
    # Examine the actual model if a Regression output
    if (inherits(model, "Regression"))
        return(Recall(model[["original"]]))
    # Examine distribution family if model is a GLM
    if (inherits(model, "glm")) {
        model.family <- family(model)[["family"]]
        if (startsWith(model.family, "Negative")) return("NBD")
        return(switch(model.family,
                      quasibinomial = ,
                      binomial = "Binary Logit",
                      gaussian = "Linear",
                      poisson = "Poisson",
                      quasipoisson = "Quasi-Poisson"))
    }
    # Needs to occur after
    if (inherits(model, "lm"))
        return("Linear")
    if (inherits(model, "multinom"))
        return("Multinomial Logit")
    # Must be polr from here
    "Ordered Logit"
}
