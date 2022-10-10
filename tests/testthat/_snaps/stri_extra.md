# does not truncate to a length shorter than elipsis

    Code
      stri_trunc("foobar", 2)
    Error <simpleError>
      `width` is shorter than `ellipsis`FALSE
    Code
      stri_trunc("foobar", 3, ellipsis = "....")
    Error <simpleError>
      `width` is shorter than `ellipsis`FALSE

