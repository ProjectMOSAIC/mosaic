if(require(manipulate)) {
  manipulate(
	histogram( ~ eruptions, data=faithful, n=n),
	n = slider(5,40)
  )
} else {
  histogram( ~ eruptions, data=faithful, n=15)
}
