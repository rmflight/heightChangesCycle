#' eye detection
#' 
#' Runs eye detection on an image using `opencv`, checks that the eye locations
#' are within a face, and then extracts their locations, and calculates an average
#' height of the eyes to be used for other things.
#' 
#' @param img the image to use
#' @export
#' @return data.frame
detect_eyes = function(img){
  eye_data = opencv::ocv_eyes(opencv::ocv_read(img))
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
  
  keep_eyes
}
