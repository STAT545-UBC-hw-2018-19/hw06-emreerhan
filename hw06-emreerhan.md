R Notebook
================
Emre Erhan
2018-11-09

## Import required packages

``` r
library(stringr)
library(stringi)
library(gapminder)
library(ggplot2)
library(dplyr)
library(testthat)
```

## 1\. Write exercises from the [Strings](https://r4ds.had.co.nz/strings.html) chapter of R for Data Science

### 14.2.5 String basics

#### In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?

`paste0` does not include a delimiter when concatenating strings,
whereas `paste` uses a space. The equivalent is `str_c`.

``` r
str_c(c("a", NA, "b"), "blah")
```

    ## [1] "ablah" NA      "bblah"

``` r
paste(c("a", NA, "b"), "blah")
```

    ## [1] "a blah"  "NA blah" "b blah"

`paste` converts the NA into a string, whereas str\_c keeps the NA as a
missing
output.

#### In your own words, describe the difference between the sep and collapse arguments to str\_c().

The `sep` parameter specifies the delimiter between elements of the
vector, whereas the `collapse` parameter specifies a string as a
delimiter to combine the vectors
themselves.

#### Use str\_length() and str\_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?

If the string is of even length, just return the char that is on the
former half of the string.

``` r
middle_char = function(string){
  mid_index = ceiling(str_length(string) / 2) # Use the ceiling function for the case where the string is of odd length.
  mid_char = str_sub(string, mid_index, mid_index)
  return(mid_char)
}
```

``` r
middle_char(c("abc", "abcd", "abcdef"))
```

    ## [1] "b" "b" "c"

#### What does str\_wrap() do? When might you want to use it?

`str_wrap` implements a paragraph wrapping algorithm called the
“Knuth-Plass algorithm”. This finds an optimal wrapping for a string
given a specified line width (in characters). This is useful for
printing long strings in a nice manner.

#### What does str\_trim() do? What’s the opposite of str\_trim()?

`str_trim` removes the whitespace (e.g. spaces, tabs, new lines) from
the start and end of a
string.

#### Write a function that turns (e.g.) a vector c(“a”, “b”, “c”) into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.

``` r
format_string = function(string_vect){
  if (length(string_vect) == 1){
    return(string_vect[1])
  }
  first_part = string_vect[1:length(string_vect)-1]
  first_part_string = str_c(first_part, collapse=', ')
  second_part = string_vect[length(string_vect)]
  second_part_string = str_c(", and ", second_part[1])
  return(str_c(first_part_string, second_part_string, sep=""))
}
```

``` r
format_string(c('a'))
```

    ## [1] "a"

``` r
format_string(c('a', 'b'))
```

    ## [1] "a, and b"

``` r
format_string(c('a', 'b', 'c'))
```

    ## [1] "a, b, and c"

### 14.3.1 Matching patterns with regex

#### Explain why each of these strings don’t match a `\`: `"\"`, `"\\"`, `"\\\"`.

  - `"\"` matches for the character `"` literally twice, since the `"`
    appears first, and then is escaped by `\`
  - `"\\"` matches for the character `"`, then `\` since it’s escaped,
    and then `"`. It would match the string `"\"`
  - `"\\\"` matches the exact same sequence as above, since the only
    difference is that the last `"` character is
escaped

#### How would you match the sequence `"'\`?

`"\'\\`

#### What patterns will the regular expression `\..\..\..` match? How would you represent it as a string?

It matches `.*.*.*` where the `*` can be any
character.

### 14.3.2 Anchors

#### How would you match the literal string “\(^\)”?

``` r
str_view("$^$", '\\$')
```

<!--html_preserve-->

<div id="htmlwidget-7758d027c0eed72c0867" class="str_view html-widget" style="width:960px;height:100%;">

</div>

<script type="application/json" data-for="htmlwidget-7758d027c0eed72c0867">{"x":{"html":"<ul>\n  <li><span class='match'>$<\/span>^$<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

### 14.7 stringi

#### Find the stringi functions that:

##### Count the number of words.

``` r
sentence = "This is a sentence with seven words."
stri_count(sentence, fixed=" ") + 1
```

    ## [1] 7

##### Find duplicated strings.

``` r
string_vector = c("these", "are", "are", "words", "to", "test", "test")
dup_index = stri_duplicated(string_vector)
string_vector[dup_index]
```

    ## [1] "are"  "test"

“are” and “test” are the duplicated
    words

##### Generate random text.

``` r
stri_rand_strings(20, 20)
```

    ##  [1] "SLo9kvqA2PD5tDemuttK" "oymxAyieJXErtktpxfDc" "fTuPqQ0a1mx74ozHOAJB"
    ##  [4] "0QLNQaDEsaRMgvKSfapg" "v7wqOfNDrPXmoJ6ovaQE" "HLlrBBWZkmJLFyHUIBXR"
    ##  [7] "xzAvLLK7cNJbbl3O06Pl" "9BIoGzV6lYWRjaMbikuZ" "NsGzbJli6XFLlkLr94ZG"
    ## [10] "PxbMfi0jDay5IbLFKRUl" "Yamz2xPEwKKHVlQzyiG4" "9yrT0zJf6eJj04Ll5lq2"
    ## [13] "2ftQO3n8EdvPKLFtJdGL" "esdVzfI31JGjpENtXKzz" "zFC9iQTr0P5urrJtIbPu"
    ## [16] "DtaYTRBlD91KJYWtytHK" "Vit4WKZBfQbj8dVmAx34" "tSXPEOVYH3wUo0HTCD6C"
    ## [19] "u171WO4ldwgkwRd9PfPR" "onWO5xSZdnbCQ9bZfXDO"

#### How do you control the language that stri\_sort() uses for sorting?

The local parameter specifies the
    language.

``` r
stri_sort(letters, local = 'haw')
```

    ##  [1] "a" "e" "i" "o" "u" "b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p"
    ## [18] "q" "r" "s" "t" "v" "w" "x" "y" "z"

``` r
str_sort(letters, local = 'en')
```

    ##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
    ## [18] "r" "s" "t" "u" "v" "w" "x" "y" "z"

## 2\. Writing useful functions

In computational biology, when working with DNA sequences, it is often
useful to get the “canonical” version of a string of DNA bases. The
canonical version is either the lexographically maximal or
lexographically minimal complement of the sequence. I will write a
function that returns the canonical sequence for a given DNA sequence.

### Preliminary functions

Let’s start by writing a function that returns the complementary
sequence of a DNA sequence.

``` r
complementary = function(seq){
  complement = chartr("ATGCN", "TACGN", seq) # chartr maps characters to characters. We use this to determine the complementary basepairs
  return(complement)
}
```

We’ll use this complementary function in the canonical function

``` r
canonical = function(seq){
  complement = complementary(seq)
  if (seq > complement){
    return(seq)
  }
  else{
    return(complement)
  }
}
```

### Preliminary tests

Let’s give it a shot\! The canonical sequence of “ACGT” and “TGCA”
should both be the same, since they’re complementary sequences. Let’s
use `test_that`

``` r
test_that("Testing if the complement function works", {
  expect_equal(complementary("ACGT"), "TGCA")
})

test_that("A simple test", {
  expect_equal(canonical("ACGT"), canonical("TGCA"))
})
```

### Adding optional parameters

It works\! Although, it may be useful to specify whether the maximal or
minimal complement is required. I’ll provide that option in a parameter.

``` r
canonical = function(seq, minimal = TRUE){
  complement = complementary(seq)
  if (minimal){
    return(min(seq, complement))
  }
  else{
    return(max(seq, complement))
  }
}
```

### Testing parameters

Let’s make some test cases using `test_that`

``` r
long_seq = 'ACGTCGATCGATGCTAGCTAGCTAGTCGACTATCAGTAG'
test_that("Testing complementary sequences", {
  expect_equal(canonical("ACGT"), canonical("TGCA"))
  expect_equal(canonical(long_seq), canonical(chartr("ATGC", "TACG", long_seq)))
  expect_equal(canonical(long_seq, minimal = FALSE), canonical(chartr("ATGC", "TACG", long_seq), minimal = FALSE))
})
```

### Verify input

Finally, let’s make sure that the input is a string, and a DNA sequence.

``` r
complementary = function(seq){
  if (!is.character(seq)){
    stop(paste("Expecting a string. Was given", typeof(seq)))
  }
  seq = toupper(seq)
  if (!str_detect(seq, '^[ACGT]*$')){
    stop(paste("Expecting a DNA sequence consisting of only A, C, G, T"))
  }
  
  complement = chartr("ATGCN", "TACGN", seq) # chartr maps characters to characters. We use this to determine the complementary basepairs
  return(complement)
}
```

``` r
canonical = function(seq, minimal = TRUE){
  if (!is.character(seq)){
    stop(paste("Expecting a string. Was given", typeof(seq)))
  }
  seq = toupper(seq)
  if (!str_detect(seq, '^[ACGT]*$')){
    stop(paste("Expecting a DNA sequence consisting of only A, C, G, T"))
  }
  
  complement = complementary(seq)
  
  if (minimal){
    return(min(seq, complement))
  }
  else{
    return(max(seq, complement))
  }
}
```

Let’s test the error
    messages.

``` r
complementary(123)
```

    ## Error in complementary(123): Expecting a string. Was given double

``` r
canonical(123)
```

    ## Error in canonical(123): Expecting a string. Was given double

``` r
canonical("Testing a non-DNA sequence")
```

    ## Error in canonical("Testing a non-DNA sequence"): Expecting a DNA sequence consisting of only A, C, G, T

Excellent\! We now have a `canonical` and `complementary` function that
can be used in a custom package.
