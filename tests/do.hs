f =
  do
    let a = 10
    q <- get
    put 10
    return 10
