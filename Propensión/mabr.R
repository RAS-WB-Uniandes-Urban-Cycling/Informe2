mabr <- function(layer) {
  stopifnot(all(!st_is(layer,"POINT")))
  
  n <- nrow(layer)
  layer <- layer %>% dplyr::filter(st_is_valid(.))
  
  if(n != nrow(layer)){
    warning(paste("Se encontraron",n-nrow(layer),"registros no validos. Para estos registros no se calculará el rectángulo de mínima area.")) 
  }
  
  crs <- st_crs(layer)
  ch <- st_geometry(st_convex_hull(layer))
  st_set_geometry(layer,NULL)
  
  sfc <- st_as_sfc(sapply(ch,FUN = function(x) {
    
    xy <- st_coordinates(x)[,c(1,2)]
    
    ## rotating calipers algorithm using the convex hull
    H    <- chull(xy)      ## hull indices, vertices ordered clockwise
    n    <- length(H)      ## number of hull vertices
    hull <- xy[H, ]        ## hull vertices
    
    ## unit basis vectors for all subspaces spanned by the hull edges
    hDir  <- diff(rbind(hull, hull[1, ])) ## hull vertices are circular
    hLens <- sqrt(rowSums(hDir^2))        ## length of basis vectors
    huDir <- diag(1/hLens) %*% hDir       ## scaled to unit length
    
    ## unit basis vectors for the orthogonal subspaces
    ## rotation by 90 deg -> y' = x, x' = -y
    ouDir <- cbind(-huDir[ , 2], huDir[ , 1])
    
    ## project hull vertices on the subspaces spanned by the hull edges, and on
    ## the subspaces spanned by their orthogonal complements - in subspace coords
    projMat <- rbind(huDir, ouDir) %*% t(hull)
    
    ## range of projections and corresponding width/height of bounding rectangle
    rangeH  <- matrix(numeric(n*2), ncol=2)  ## hull edge
    rangeO  <- matrix(numeric(n*2), ncol=2)  ## orthogonal subspace
    widths  <- numeric(n)
    heights <- numeric(n)
    
    for(i in seq(along=numeric(n))) {
      rangeH[i, ] <- range(projMat[  i, ])
      
      ## the orthogonal subspace is in the 2nd half of the matrix
      rangeO[i, ] <- range(projMat[n+i, ])
      widths[i]   <- abs(diff(rangeH[i, ]))
      heights[i]  <- abs(diff(rangeO[i, ]))
    }
    
    ## extreme projections for min-area rect in subspace coordinates
    ## hull edge leading to minimum-area
    eMin  <- which.min(widths*heights)
    hProj <- rbind(   rangeH[eMin, ], 0)
    oProj <- rbind(0, rangeO[eMin, ])
    
    ## move projections to rectangle corners
    hPts <- sweep(hProj, 1, oProj[ , 1], "+")
    oPts <- sweep(hProj, 1, oProj[ , 2], "+")
    
    ## corners in standard coordinates, rows = x,y, columns = corners
    ## in combined (4x2)-matrix: reverse point order to be usable in polygon()
    ## basis formed by hull edge and orthogonal subspace
    basis <- cbind(huDir[eMin, ], ouDir[eMin, ])
    hCorn <- basis %*% hPts
    oCorn <- basis %*% oPts
    pts   <- t(cbind(hCorn, oCorn[ , c(2, 1)]))
    
    ## angle of longer edge pointing up
    dPts <- diff(pts)
    e    <- dPts[which.max(rowSums(dPts^2)), ] ## one of the longer edges
    eUp  <- e * sign(e[2])       ## rotate upwards 180 deg if necessary
    deg  <- atan2(eUp[2], eUp[1])*180 / pi     ## angle in degrees
    
    st_sfc(st_polygon(list(rbind(pts,pts[1,]))))
  }),crs = crs)

  st_geometry(layer) <- sfc 
  
  return(layer)
}
