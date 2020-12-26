#' eye detection
#' 
#' Runs eye detection on an image using `opencv`, checks that the eye locations
#' are within a face, and then extracts their locations, and calculates an average
#' height of the eyes to be used for other things.
#' 
#' @param img the image to use
#' @export
#' @return list
detect_eyes = function(img){
  if (inherits(img, "character")) {
    img = opencv::ocv_read(img)
  }
  eye_data = opencv::ocv_eyes(img)
  eye_locs = attr(eye_data, "eyes")
  face_locs = attr(eye_data, "faces")
  
  eye_match = purrr::map(seq(1, nrow(face_locs)), function(in_loc){
    start = (in_loc - 1) * 2
    match_locs = start + seq(1, 2)
    use_eyes = eye_locs[match_locs, ]
    if (any(use_eyes$radius == 0)) {
      use_eyes = NULL
    } else if (nrow(use_eyes) != 2) {
      use_eyes = NULL
    }
    
    use_eyes
  })
  
  keep_eyes = eye_match[purrr::map_lgl(eye_match, ~!is.null(.x))]
  
  if (length(keep_eyes) > 1) {
    stop("More than one set of eyes found!")
  }
  
  if (length(keep_eyes) == 0) {
    stop("No sets of eyes found!")
  }
  keep_eyes = keep_eyes[[1]]
  
  # check that the eye radii are within 10% of each other
  rad2_1 = keep_eyes$radius[2] / keep_eyes$radius[1]
  if (!dplyr::between(rad2_1, 0.9, 1.1)) {
    stop("Eye radii are different by more than 10%, stopping!")
  }
  
  list(image = img, eyes = keep_eyes)
}

#' rectangle detection
#' 
#' Given an image file, detects rectangles, tries to find the two we are actually interested in
#' , makes sure that they are horizontal, and 
#' returns their x and y locations.
#' 
#' @param img the location of the image or a pointer from `ocv_read`
#' @export
#' @return list
detect_rectangles = function(img){
  if (inherits(img, "character")) {
    img = opencv::ocv_read(img)
  }
  rect_data = opencv::ocv_rectangle(img)
  rect_locs = attr(rect_data, "rectangles")
  rect_df = purrr::map(rect_locs, ~ as.data.frame(.x))
  
  summarize_horizontal = function(rect_points){
    y_range = abs(diff(range(rect_points$y)))
    x_range = abs(diff(range(rect_points$x)))
    data.frame(x = mean(rect_points$x),
               width = x_range,
               y = mean(rect_points$y),
               height = y_range,
               horizontal = y_range <= x_range)
  }
  
  horizontal_rects = purrr::map_df(rect_df, summarize_horizontal)
  horizontal_rects = dplyr::filter(horizontal_rects, horizontal)
  
  # if we did this right, the biggest source of variance should be the x location,
  # so we will cheat and use principal components to cluster them by their scores
  pc_rects = prcomp(as.matrix(horizontal_rects[, c("x", "width", "y", "height")]), center = TRUE, scale. = FALSE)
  
  rect_dists = dist(as.matrix(horizontal_rects[, c("x", "width", "y", "height")]))
  rect_clust = hclust(rect_dists)
  cut_dist = max(rect_clust$height) * 0.9
  horizontal_rects$cluster = cutree(rect_clust, h = cut_dist)
  
  summary_rects = dplyr::group_by(horizontal_rects, cluster) %>%
    dplyr::summarize(med_x = median(x),
                     med_width = median(width),
                     med_y = median(y),
                     med_height = median(height),
                     .groups = "drop")
  
  # now we check if there are two or more rectangles.
  # If there are more than two, we search for two parallel ones with roughly
  # the same x and y
  rect_lims = dplyr::group_by(summary_rects, cluster) %>%
    dplyr::summarise(low_x = med_x - (0.1 * med_x),
                     hi_x = med_x + (0.1 * med_x),
                     low_width = med_width - (0.1 * med_width),
                     hi_width = med_width + (0.1 * med_width),
                     low_height = med_height - (0.1 * med_height),
                     hi_height = med_height + (0.1 * med_height),
                     .groups = "drop")
  if (nrow(summary_rects) > 2) {
    find_matches = purrr::map(seq(1, nrow(rect_lims)), function(in_lims){
      which(dplyr::between(summary_rects$med_x, rect_lims$low_x[in_lims], rect_lims$hi_x[in_lims]) &
        dplyr::between(summary_rects$med_width, rect_lims$low_width[in_lims], rect_lims$hi_width[in_lims]) &
        dplyr::between(summary_rects$med_height, rect_lims$low_height[in_lims], rect_lims$hi_height[in_lims]))
                       
    }) %>% unique()
    
    has_2 = find_matches[purrr::map_lgl(find_matches, ~ length(.x) == 2)]
    
    if (length(has_2) != 1) {
      stop("Can't find just 2 parallel rectangles, stopping!")
    }
    summary_rects = summary_rects[has_2[[1]], ]
  } else if (nrow(summary_rects) == 2) {
    check_matches = purrr::map(seq(1, nrow(rect_lims)), function(in_lims){
      which(dplyr::between(summary_rects$med_x, rect_lims$low_x[in_lims], rect_lims$hi_x[in_lims]) &
              dplyr::between(summary_rects$med_width, rect_lims$low_width[in_lims], rect_lims$hi_width[in_lims]) &
              dplyr::between(summary_rects$med_height, rect_lims$low_height[in_lims], rect_lims$hi_height[in_lims]))
      
    })
    has_both = purrr::map_lgl(check_matches, ~ length(.x) == 2)
    if (sum(has_both) != 2) {
      stop("The two rectangles are too different in size and location, stopping!")
    }
  } else if (nrow(summary_rects) == 1) {
    stop("Only one rectangle found, we need two. Stopping!")
  }
  summary_rects$cluster = NULL
  list(image = img, rectangles = as.data.frame(summary_rects))
}
