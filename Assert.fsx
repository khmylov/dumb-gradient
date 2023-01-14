module Assert
  let inline eq expected actual =
    if expected <> actual then failwithf "Expected %A = %A" expected actual

  let inline ok expected = eq expected true
