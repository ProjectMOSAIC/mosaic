if(require(manipulate)) {
  manipulate(
	histogram( ~ eruptions, data=faithful, n=n),
	n = slider(5,40)
  )
} else {
  message("This demo requires the manipulate package available in RStudio.")
}
