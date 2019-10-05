makeHiddenBoard <- function(height, width, mines){
  # Initialize with zeros
  hiddenBoard <- matrix(data = 0,
                        nrow = height,
                        ncol = width)
  
  # Randomly pick #mines indices
  mineIndices <- sample(x = height * width,
                        size = mines,
                        replace = FALSE)
  
  # Set all mines to negative numbers
  hiddenBoard[mineIndices] <- -10 # 9 (neighbors + self) + 1 to remain negative
  
  for(r in 1:height){
    for(c in 1:width){
      # If a mine
      if(hiddenBoard[r, c] < 0){
        # For all cells in a 3x3 square (except for border cases)
        for(rNew in max(r - 1, 1):min(r + 1, nrow(hiddenBoard))){
          for(cNew in max(c - 1, 1):min(c + 1, ncol(hiddenBoard))){
            # Add one to the board count
            hiddenBoard[rNew, cNew] <- hiddenBoard[rNew, cNew] + 1
          }
        }
      }
    }
  }
  
  # Return the hiddenBoard changed so all negatives are the same
  hiddenBoard[hiddenBoard < 0] <- -1
  
  # Return the hiddenBoard
  hiddenBoard
}

reveal <- function(hidden, mask, r, c){
  # Set the current mask entry to "show" (1, 0 is hide)
  mask[r, c] <- 1
  
  # If in a hollow zone
  if(hidden[r, c] == 0){
    # For all cells in a 3x3 square (except for border cases)
    for(rNew in max(r - 1, 1):min(r + 1, nrow(mask))){
      for(cNew in max(c - 1, 1):min(c + 1, ncol(mask))){
        # If not the central cell or already revealed
        if(!(rNew == r && cNew == c) & mask[rNew, cNew] != 1){
          # Reveal the neighbor
          mask <- reveal(hidden, mask, rNew, cNew)
        }
      }
    }
  }
  
  # Always return the mask
  mask
}
# 
# printBoard <- function(hidden, mask){
#   # Loop over rows
#   output <- ""
#   for(r in 1:nrow(mask)){
#     for(c in 1:ncol(mask)){
#       # For each cell
#       output <- switch(mask[r, c],
#                        # If still covered print '.'
#                        "0" = paste0(output, "."),
#                        
#                        # Otherwise print ' ' or the number
#                        "1" = if(hidden[r, c] == 0){ paste0(output, " ")}
#                             else                { paste0(output, hidden[r, c])},
#                        
#                        # Flag functionality to be implemented
#                        "2" = paste0(output, "f"))
#     }
#     output <- paste0(output, "\n")
#   }
#   writeLines(output)
# }

imageBoard <- function(hidden, mask){
  toGraph <- mask
  # Reverse and transpose (equivalent of rotating clockwise once, how image() accepts it)
  toGraph <- t(apply(toGraph, 2, rev))
  
  # Plot the image
  image(toGraph,
        zlim = c(-1, 8),
        col = hcl.colors(10, palette = "RdPu"))
}

click <- function(hidden, mask, r, c){
  # If a valid click
  if(r >= 1 & r <= nrow(mask) &
     c >= 1 & c <= ncol(mask)){
    # If not a mine
    if(hidden[r, c] >= 0){
      # Process and display click
      mask <- reveal(hidden, mask, r, c)
      printBoard(hidden, mask)
    } else {
      print("Game over")
    }
  } else {
    print("Invalid click")
  }
  
  # Return mask
  mask
}

# To play
height <- 16
width <- 30
mines <- 99
hiddenBoard <- makeHiddenBoard(height, width, mines)
mask <- matrix(data = 0,
               nrow = height,
               ncol = width)

# Run this repeatedly
mask <- click(hiddenBoard, mask, 1, 1)

