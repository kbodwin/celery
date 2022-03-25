#-------- SSE -------#

#' Calculates Sum of Squared Error in each cluster
#'
#' @param object a fitted kmeans celery model
#' @param ... Other arguments passed to methods.
#'
#' @return A tibble with two columns, the cluster name and the SSE within that
#' cluster.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   within_cluster_sse()
#'
#' @export
within_cluster_sse <- function(object, ...) {

  obj_sum <- extract_fit_summary(object)

  res <- tibble::tibble(
    .cluster = unique(extract_cluster_assignment(object)$.cluster),
    orig_label = unique(obj_summ$orig_label)
  ) %>%
    arrange(orig_label) %>%
    mutate(
      sse = obj_summ$within_sse
    ) %>%
    arrange(.cluster) %>%
    select(-orig_label)

  return(res)

}


#' Compute the sum of within-cluster SSE
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_wss()
#' @export
tot_wss <- function(object, ...) {

  sum(extract_fit_summary(object)$within_sse)

}

#' Compute the total sum of squares
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   tot_sse()
#' @export
tot_sse <- function(object, ...) {

  extract_fit_summary(object)$tot_sse

}



#' Compute the ratio of the WSS to the total SSE
#'
#' @param object An cluster_spec object.
#' @param ... Other arguments passed to methods.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' kmeans_fit %>%
#'   sse_ratio()
#' @export
sse_ratio <- function(object, ...) {

  tot_wss(object)/tot_sse(object)

}



#-------- Silhouette -------#

#' Measures silhouettes between clusters
#'
#' @param .data A data frame
#' @param clusters The column containing cluster assignments.
#' @param distance The distance metric to use. (Currently only "euclidean")
#'
#' @return The silhouettes matrix.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' mtcars %>%
#'   augment(kmeans_fit) %>%
#'   silhouettes(.cluster)
#'
#' @export
silhouettes <- function(.data, clusters,
                        distance = "euclidean") {

  mat <- .data %>%
    select(-{{clusters}}) %>%
    as.matrix() %>%
    as.numeric()

  .data %>%
    pull({{clusters}}) %>%
    cluster::silhouette(dist = dist(mat, method = distance))

}


#' Measures average silhouette between clusters
#'
#' @param .data A data frame
#' @param clusters The column containing cluster assignments.
#' @param distance The distance metric to use. (Currently only "euclidean")
#'
#' @return The average of all cluster silhouettes.
#'
#' @examples
#' kmeans_spec <- k_means(k = 5) %>%
#'   set_engine_celery("stats")
#'
#' kmeans_fit <- fit(kmeans_spec, ~., mtcars)
#'
#' mtcars %>%
#'   augment(kmeans_fit) %>%
#'   silhouettes(.cluster)
#'
#' @export
avg_silhouette <- function(.data, clusters,
                           distance = "euclidean") {

  mean(silhouettes(.data, {{clusters}}, distance = "euclidean"))

}

#-------- Gap Method -------#

#-------- Enrichment -------#

#' Measures relationship between cluster assignments and another categorical variable.
#'
#' @param data the dataset
#' @param clusters the variable with cluster assignments
#' @param ...  other variables for enrichment
#'
#' @return The p-value of a Chi-Square test for relationship between cluster
#' assignments and the categorical variable.

# this needs to be ... instead of var soon
#' @export
enrichment <- function(data, clusters, var) {

  res <- list()
  vec <- data %>% pull({{var}})

  if (!is.numeric(vec)) {

    res <- data %>%
      janitor::tabyl({{clusters}}, {{var}}) %>%
      select(-1) %>%
      as.matrix() %>%
      chisq.test() %>%
      tidy()

  } else {

    ### anova

  }


  return(-log(res$p.value))

}

