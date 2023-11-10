# does not truncate to a length shorter than elipsis

    Code
      stri_trunc("foobar", 2)
    Condition
      Error in `stri_trunc()`:
      ! `width` is shorter than `ellipsis`FALSE
    Code
      stri_trunc("foobar", 3, ellipsis = "....")
    Condition
      Error in `stri_trunc()`:
      ! `width` is shorter than `ellipsis`FALSE

