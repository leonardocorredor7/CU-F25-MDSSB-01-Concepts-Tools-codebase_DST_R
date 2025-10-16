##############################################
#### F23_MDSSB-MET-01-A_Data Science Tools in R
####
#### Session 6: Strings, Characters and Regular Expressions
####
#### Recommended Reading: R for Data Science
#### Chapter on Strings 
#### 
#### see also: Python Data Science Handbook
#### chapter 3, section: Vectorized String Operations
####
####  ... and if you like this, you're going to love ...
####  https://www.tidytextmining.com/
####
#### Armin Müller, Constructor University
##############################################



# Characters / Strings


# Strings with stringr

library(tidyverse)

# Avoiding base-R functions
# string functions. str_...()



# 1. Basics

# To create a character vector (or string), 
# we need to enclose strings (sequences of characters) with quotes: 
string1 <- "This is a string"
string1a <- 'This is also a string'

typeof(string1)
class(string1)

# standard output
string1
print(string1a)

# create a vector from 2 strings
c(string1, string1a)

# write Text lines to a connection
?writeLines()
writeLines(string1) # single string
writeLines(c(string1, string1a)) # string vector


# concatenate
?cat()
cat(string1)
cat(c(string1, string1a))

# 
string2 <- 'If I want to include a "double quote" inside a string, I use single quotes.'
string2a <- "If I want to include a 'single quote' inside a string, I use double quotes."
writeLines(c(string2, string2a)) 


#string3 <- "This is a string with no closed quote

# We also use a backslash to tell R that the quote should not encode a string.
# The backslash is used as an escape character.
double_quote <- "\"" # or '"' 
double_quote # visible backslash
writeLines(double_quote) 
cat(double_quote) 

string2b <- "If I want to include a \"double quote\" inside a \'string\', I use single quotes."
cat(string2b)

single_quote <- '\'' # or "'"
single_quote # no visible backslash
writeLines(single_quote)
cat(single_quote)


backtick <- '\`' 
backtick # no visible backslash
writeLines(backtick)



# Given that the backslash prevents other characters from having a function:
# How do we enclose a backslash in a string?
x <- c("\"", "\\")
print(x)
writeLines(x)

x2 <- c("\"", "\\", '\'', '\\')
print(x2)
writeLines(x2)


# What other special characters are there?
# \n: new line
# \t: tab
?'"' # see complete list !!!

Blake <- c(" Tyger, Tyger, burning bright, \n \v In the forests of the night; \n What immortal hand or eye, \n \v Could frame thy fearful symmetry?")
print(Blake)
writeLines(Blake)


# How can we display non-English characters?
# in Unicode
x <- "\u00b5" # (UTF-16 (hex) 	0x00B5 (00b5))
x
writeLines("\u00b5")

# Based on "你" (U+4F60) and  “好” (U+597D) = Hello in Chinese
writeLines(c("\u4F60", "\u597D"))
cat(c("\u4F60", "\u597D"))




# 1.1 check string length
?str_length()
str_length(c("one", "two", "three"))
str_length(c("a", "R for data science", NA))

HELLO <- c("Hello", "你好", "أهلاً")
str_length(HELLO)


# 1.2 combining strings
?str_c()
str_c("Hello", "World")
# see also:
paste0("Hello", "World")
paste("Hello", "World")

str_c("Hello", " ", "World!")

str_c("\u4F60", "\u597D","!")

## same as
paste0("\u4F60", "\u597D")
print(paste0("\u4F60", "\u597D"))
writeLines(paste0("\u4F60", "\u597D"))

# BUT NOT:
str_c("\u4F60", "\u597D", NA)
# even though ...
paste0("\u4F60", "\u597D", NA)
print(paste0("\u4F60", "\u597D", NA))



# sep argument
str_c("Hello", "World!", sep = " ")
str_c("Hello", "World!", "Hello", "again", "...", sep = " ")
paste("Hello", "World!", sep = " ")
# but not
paste0("Hello", "World", sep = ", ") # separator not allowed!


## a.) vectorization
# str_c() is vectorised &  recycles shorter vectors to the same length as the longest:
str_c("prefix-", 
      c("a", "b", "c", "d"), 
      "-suffix")
seq(1, 36, 1)
# potential use case for web scraping
str_c("www.your_website.com/index_", seq(1, 100, 1))

# Missing values
x <- c("abc", "def", NA) 
str_c("|-", 
      x, 
      "-|")

# treat NA as a string:
?str_replace_na()
str_c("|-", 
      str_replace_na(x, replacement = "not available"), 
      "-|")



## b. Silently dropping objects with if()
name <- "Hadley"
name2 <- "Mr. President"
time_of_day <- "morning"
time_of_day2 <- "afternoon"
birthday <- FALSE # logical condition, boolean value
birthday2 <- T

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  ".")

str_c("Good ", time_of_day2, " ", name2,
  if (birthday2) " and HAPPY BIRTHDAY",
  ".")


## c. collapsing
# collapse option to collapse vector to a string
str_c(c("x" , "y", "z"))
length(str_c(c("x" , "y", "z")))

str_c(c("x" , "y", "z"), collapse = ", ")
length(str_c(c("x" , "y", "z"), collapse = ", "))

# Be mindful of the difference: sep & collapse in str_c()
# sep: 	String to insert between input vectors.
# collapse: Optional string used to combine output into single string
?str_c()
str_c("x" , "y", "z") # joins 3 vectors (1 observation each) into one vector with 1 observation
length(str_c("x" , "y", "z") )
str_c("x" , "y", "z", sep = ", ") # same as above, but with "," separator
str_c("x" , "y", "z", collapse = ", ") # same as str_c("x" , "y", "z")

str_c(c("x" , "y", "z")) # enter 1 vector with 3 observations
length(str_c(c("x" , "y", "z"))) # get 1 vector with 3 observations
str_c(c("x" , "y", "z"), sep = ", ") # 3 observations of single vector, no ","
str_c(c("x" , "y", "z"), collapse = ", ") # [1] "x, y, z"

# combine 2 vectors:
str_c(c("x" , "y", "z"), c("a", "b", "c")) # enter 2 vectors with 3 observations
length(str_c(c("x" , "y", "z"), c("a", "b", "c"))) # get 1 vector with 3 observations
str_c(c("x" , "y", "z"), c("a", "b", "c"), collapse = ", ") # get 1 vector with 1 observation

# But not: differing lengths
str_c(c("x" , "y"), c("a" , "c", "g", "h")) #



# 1.3 Subsetting strings

# get first 3 letters of a string
?str_sub()
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3) # string, start, end

# get last 3 letters
str_sub(x, -3, -1)

# string too short --> print the maximum possible amount
str_sub("Pea", 1, 5)


# use the assignment form of str_sub() to modify strings:
?str_to_lower()
?str_sub()

# only convert first letter to lower case
str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

# convert everything to upper case
x <- str_to_upper(x)
x


str_to_upper("Rio De Janeiro")
str_to_lower("Rio De Janeiro")


# 1.4 Locales (countries)

# changing cases: affected by locale
# alternative functions
str_to_upper(c("i", "ı"))
str_to_title(c("i", "ı"))

# different rules for different languages
# specify locale to resolve 
str_to_upper(c("i", "ı"), locale = "tr")


# (Latin Alphabet and Latinizations only)
str_to_upper(c("i", "ı"), locale = "ara") # Arabic - no capitalization except for Latin letters
str_to_upper(c("i", "ı"), locale = "zh") # Chinese - no capitalization except for Latin letters
str_to_upper(c("a", "o"), locale = "el") # Greek
str_to_upper("\u00b5", locale = "el")
str_to_lower("\u00b5")
# ISO language code: 2-3 letter abbreviation

# List: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes

# sorting: affected by locale (due to different order of alphabet)
x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")  # English
str_sort(x, locale = "haw") # Hawaiian
str_sort(x, locale = "tr") # Turkish





# 1.5 some other usefull functions

# a. formats a long string to a paragraph
# str_wrap()
?str_wrap()
## create a long string
paragraph <- str_c(rep("This is a long string that needs to be wrapped to fit within a specific width. ",
                       3), 
                   collapse = "")
paragraph
## then reformat it
paragraph %>% 
  str_wrap(width = 70) %>% # introduce line breaks
  cat(sep = "\n") # concatenate and print




# str_trim()
?str_trim()
# removes white space from start and end of a string

text_with_whitespace <- "   This is a text with leading and trailing spaces.   "
text_with_whitespace
text_with_whitespace %>% 
 str_trim()


trimmed_text <- text_with_whitespace %>% str_trim()
trimmed_text

# Adding white space to a string
# Revert str_trim() using str_pad()
?str_pad() # to add white space
?nchar()
original_width <- nchar(text_with_whitespace)
original_width

# compare to trimmed text
nchar(trimmed_text)

padded_text <- str_pad(trimmed_text, width = original_width, 
                                 side = "both", 
                                 pad = "$")
padded_text
text_with_whitespace




# 2 Matching patterns with regular expressions

# A regular expression (shortened as regex or regexp; sometimes 
# referred to as rational expression) is a sequence of characters 
# that specifies a search pattern in text. Usually such patterns are used 
# by string-searching algorithms for "find" or "find and replace" operations 
# on strings, or for input validation. Regular expression techniques are 
# developed in theoretical computer science and formal language theory. 
# Source: https://en.wikipedia.org/wiki/Regular_expression



# 2.1 Basics
?str_view()

# simplest way: exact matching
x <- c("apple", "banana", "pear")
str_view(x, "an") # gives you the first match

class(str_view(x, "an"))
typeof(str_view(x, "an"))


# Functional characters for matching:
# "." matches any character
str_view(x, ".a.")

# how to match an actual point?
# not a problem for regular strings
writeLines(".")
cat(".")

# But: to create the regular expression, we need \\ (in R)
# In R, regular expressions are often written as string literals (i.e. enclosed in quotes).
# The backslash serves as an escape character in the strings.
# This is not (necessarily always) the same in Python.

## regular expressions use \ to escape special behavior
## strings do the same, so you need 2 of them
dot <- "\\." # Python: re.compile("\.")
# But the expression itself only contains one:
writeLines(dot)

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "\\.")
str_view(c("abc", "a.c", "bef"), ".")

str_view(c("abc", "a.c", "bef"), regex("\\."))



# so how to match an actual "\"?
# you need to escape it, creating the regular expression \\ ...
# To create that regular expression, you need to use a string, 
# which also needs to escape \ ...
writeLines("\\") # remember
# That means to match a literal \ you need to write "\\\\" — 
# you need four backslashes to match one!

x <- "a\\b" # one backslash to escape the second one and treat it as part of the string
y <- "a\"b"
writeLines(x)
writeLines(y)

str_view(x, "\\\\")
str_view(y, "\"")


# how to match the sequence "'\ ?
x <- "a\"\'\\b"
x
writeLines(x)
str_view(x, "\"\'\\\\")


#  What will \..\..\.. match?
# match:  3 combinations of . + any character
x <- c("apple......", "banana", "..ba..na..na", "pear", "......pear", ".b.n.n.")
str_view(x, "\\..\\..\\..") # sequence of 3x point "." plus "anything"
# observations 1, 5 and 6 match

## compared to ...
str_view(x, "......")  # any sequence of length 6
str_view(x, "\\......") # a sequence of 1 point plus any sequence of length 5
str_view(x, "..\\....") # 2 anything, 1 point, 3 anything
str_view(x, "\\.\\.\\.") # 3 points





# 2.2 Anchors

# a normal RegEx matches any part of the string
# you might want to start at the beginning or end
# use anchors
x <- c("apple", "banana", "pear")
str_view(x, "^a") # starts with a
str_view(x, "a$") # ends with a

# match a specific word alone
x <- c("apple pie", "apple", "apple cake")
str_view(x, "^apple$") # begins and ends with apple


# how match "$^$" and "\$\^\$" ?
x <- c("\\apple\\", "banana$^$", "pear\\$\\^\\$")
# In the string, we do not need to escape ^ and $.
print(x)
cat(x)
str_view(x, "\\$\\^\\$") # Match "$^$"
str_view(x, "^\\\\")  # Match backslash at the beginning
str_view(x, "\\\\$") # Match backslash at the end
str_view(x, "\\\\\\$\\\\\\^\\\\\\$")  # Match \$\^\$



# selecting words from a corpus
?str_view()
stringr::words
class(stringr::words)
words <- stringr::words
str_view(words, "^y", match = TRUE)  # starts with y
str_view(stringr::words, "^x", match = TRUE) # begins with x
str_view(stringr::words, "x$", match = TRUE) # ends with x
str_view(stringr::words, "^....$", match = TRUE) # 4-letter words




# 2.3 Character classes and alternatives

# patterns that match more than 1 character like .
# \d: matches any digit.
# \s: matches any whitespace (e.g. space, tab, newline).
# [abc]: matches a, b, or c.
# [^abc]: matches anything except a, b, or c.

# to create a regular expression containing \d or \s, 
# you’ll need to escape the \ for the string, so you’ll type "\\d" or "\\s".
x <- c("abc", "a.c", "a*c", "a c", "a5c", "a/b", "B.c", "C*c", "e c")
str_view(x, "\\d")
str_view(x, "\\s")
str_view(x, "a[.]c") # Square brackets [.] denote a character class, but in this case only contain a literal dot.
str_view(x, ".[*]c") # here they contain a literal *
str_view(x, "a ") # a followed by space
str_view(x, "a/") # a followed by slash
str_view(x, "[a-z]") # all lower-case letters
str_view(x, "[^a-z]") # everything that is not a non-lower-case-letter
str_view(x, "^[a-z]") # lower-case letter at the beginning of a string
str_view(x, "[a-z]$") # lower-case letter at the beginning of a string

# This works for most (but not all) regex meta-characters: $ . | ? * + ( ) [ {.
# a few characters have special meaning even inside a character class 
# and must be handled with backslash escapes: ] \ ^ and -.



# Use alternation to switch between patterns
# with the OR operator "|"
str_view(c("grey", "gray"), "gr(e|a)y")


# Examples
# words that start with a vowel
str_view(stringr::words, "^(e|a|i|o|u)", match = TRUE)

#  Consonants
str_view(stringr::words, "^[^eaiou]", match = TRUE) # match words that start with consonants
str_view(stringr::words, "[^eaiou]$", match = TRUE) # end with any consonant

#  words that end with ed, but not with eed
str_view(stringr::words, "[^e]ed$", match = TRUE)
#  words that end with eed
str_view(stringr::words, "[e]ed$", match = TRUE)
str_view(stringr::words, "eed$", match = TRUE)

#  words that end with ize or ise (American and British English)
str_view(stringr::words, "ise$", match = TRUE)
str_view(stringr::words, "ize$", match = TRUE)
str_view(stringr::words, "i(z|s)e$", match = TRUE)

# let's try and be more precise:
str_view(stringr::words, ".[^aeiou]i(z|s)e$", match = TRUE)
# Words ending in -ise or -ize, preceded by a consonant and some other character
# otherwise and practice are still mismatches.
# Here we need language models to get ahead ...



#  mobile phone numbers 
str_view(stringr::words, "\\d\\d\\d\\d[/]\\d\\d\\d\\d\\d\\d\\d", match = TRUE)
## none here, but elsewhere maybe ... 




# 2.4 Repitition

# how many times does a pattern match?
# ?: 0 or 1
# +: 1 or more
# *: 0 or more


str_view(stringr::words, "^[^eaiou]+$", match = TRUE) 
# start with 1 or more consonants, and then end.
# "y" is considered a consonant.


# longest year in Roman numerals
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?") # one C (100) plus maybe a second
str_view(x, "CC+") # two Cs or more
str_view(x, "CC*") # one C or more
str_view(x, "C[LX]+") # one C plus one or more characters that are either L (50) or X


# colou?r - match both British and American English
str_view(stringr::words, "colou?r", match = TRUE)


# You can also specify the number of matches precisely:
# {n}: exactly n
str_view(x, "C{2}")
# {n,}: n or more
str_view(x, "C{2,}")
# {n,m}: between n and m
str_view(x, "C{1,2}")


#  equivalents of ?, +, * in {m,n} form.
# ? = {0,1}
# + = {1,}
# * = {0,}

# greedy matches: match the longest string possible
str_view(x, "C{2,3}")
# with ? behind: lazy, match the shortest string possible
str_view(x, "C{2,3}?")
str_view(x, "C[LX]+")
str_view(x, "C[LX]+?") # L or X once or more, lazy match
str_view(x, "C[LX]??") # L or X zero or once, lazy match


# mobile phone numbers ... simplified
str_view(stringr::words, "\\d{4,5}[/]\\d{7,8}", match = TRUE)

# other examples
str_view(stringr::words, "^[^eaiou]", match = TRUE) # start with any consonant
str_view(stringr::words, "^[^eaiou]{3}", match = TRUE) # start with any 3 consonants
str_view(stringr::words, "[eaiou]{3,}", match = TRUE) # Have three or more vowels in a row
str_view(stringr::words, "[eaiou][^eaiou]{2,}", match = TRUE) # 1 vowel + 2 or more consonants
str_view(stringr::words, "([eaiou][^eaiou]){2}", match = TRUE) # two vowel-consonant combinations in a row
str_view(stringr::words, "[eaiou][^eaiou][eaiou][^eaiou]", match = TRUE) # same
str_view(stringr::words, "([eaiou][^eaiou])\\1", match = TRUE) # repeated vowel-consonant combination
# \\1: This is a back-reference to the first capturing group ([eaiou]). 
# It matches whatever text was captured by the first capturing group.





# 2.5 Grouping and back-references

# Earlier, you learned about parentheses as a way to disambiguate complex expressions. 
# Parentheses also create a numbered capturing group (number 1, 2 etc.). 
# A capturing group stores the part of the string matched by the part of the regular expression inside the parentheses. 
# You can refer to the same text as previously matched by a capturing group with back-references,
#  like \1, \2 etc. 

str_view(fruit, "(..)\\1", match = TRUE) # repeated pair of letters
str_view(fruit, "(.)(.)\\1", match = TRUE) # 2 single letters, then repeat the first
str_view(fruit, "(.)(.)\\2", match = TRUE) # 2 single letters, then repeat the second
str_view(fruit, "(..)(.)\\1", match = TRUE) # a pair of letters followed a single letter, then repeat the first (pair)
str_view(fruit, "(..)(.)\\2", match = TRUE) # a pair of letters followed a single letter, then repeat the second (single letter)
str_view(fruit, "(..)(..)\\2", match = TRUE) # 2 pairs of letters, repeat the second
str_view(fruit, "(.)(.)(.)\\1", match = TRUE) # 3 single letters, repeat first element
str_view(fruit, "(.)(.)(.)\\2", match = TRUE) # 3 single letters,  repeat second element
str_view(fruit, "(.)(.)(.)\\3", match = TRUE) # 3 single letters,  repeat third element


#"(.)(.)\\2\\1": 2 single letters: repeat second element, then repeat first element
str_view(stringr::words, "(.)(.)\\2\\1", match = TRUE)

# "(.).\\1.\\1": repeat first element twice, with random elements in between
str_view(stringr::words, "(.).\\1.\\1", match = TRUE)
str_view(fruit, ".(.).\\1.\\1", match = TRUE)


# "(.)(.)(.).*\\3\\2\\1" # repeat first three characters in reverse order after some random characters
str_view(stringr::words, "(.)(.)(.).*\\3\\2\\1", match = TRUE) 
# (.)(.)(.): letters 1, 2, 3 [par]
# *. : something random, 0 or more times [ag]
# \\3: repeat third letter: r
# \\2: repeat second letter: a
# \\1: repeat first letter: p





# 3. Tools

# 3.1 detect matches
x <- c("apple", "banana", "pear")
str_detect(x, "e")

# TRUE becomes 1 and FALSE becomes 0 when using logical vectors in a numeric context
# good for sum() and mean()
sum(as.numeric(str_detect(x, "e"), na.rm = TRUE))
sum(str_detect(x, "e"), na.rm = TRUE)


# How many common words start with t?
sum(str_detect(words, "^t"))

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))


# complex matches: use multiple string-detect calls with logical operators
# rather than single call
# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")

# compare the results
summary(no_vowels_1 == no_vowels_2) # base R
# alternative
?identical() # also base R
identical(no_vowels_1, no_vowels_2) 


# Look at specific observations based on boolean comparison:
words[no_vowels_1 != no_vowels_2]



# select elements that match a pattern with str_subset()
words[str_detect(words, "x$")]
str_subset(stringr::words, "x$")

# real-life situation - strings in column of data-frame: filter() 
df <- tibble(
  word = words, 
  i = seq_along(word)
)
df

df2 <- df %>% filter(str_detect(word, "x$"))
df2

# str_count() counts matches
x <- c("apple", "banana", "pear")
str_count(x, "a")

# On average, how many vowels per word?
mean(str_count(words, "[aeiou]"))


# combine with mutate
df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

# matches never overlap
str_count("abababa", "aba")
str_view("abababa", "aba")



# simple calculations
# highest number of vowels and highest proportion?
df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    length = str_length(word),
    proportion = vowels/length) %>%
  arrange(desc(vowels)) %>%
  arrange(desc(proportion))





# 3.2 Extract matches

# str_extract() to extract matches

## Harvard sentences dataset in stringr package
length(sentences)
head(sentences)

# find sentences that contain a color:
colours <- c("red", "orange", "yellow", "green", "blue", "purple", "brown") ## first: create color vector
colour_match <- str_c(colours, collapse = "|") # turn into regular expression
colour_match # this is not sophisticated enough, but it will do for now ...

has_colour <- str_subset(sentences, colour_match) # select sentences that have color
has_colour # ... with some exceptions ....
# [50] "He put his last cartridge into the gun and fired."
# let's correct that
colours2 <- c("(R| r)ed", "orange", "yellow", "green", "blue", "purple", "brown")
colour_match <- str_c(colours2, collapse = "|") # turn into regular expression
has_colour <- str_subset(sentences, colour_match) 
has_colour # better

matches <- str_extract(has_colour, colour_match) # extract color to see which one it is
head(matches)

# ?str_extract() #only extracts the first match
# take the sentences with more than one color mentioned
more <- sentences[str_count(sentences, colour_match) > 1]
# extract the color and check
str_view(more, colour_match)
str_extract(more, colour_match)

# get all matches in a list
str_extract_all(more, colour_match) %>% 
  unlist()

str_extract_all(more, colour_match, simplify = TRUE) # output: matrix

# How does it deal with different match length?
x <- c("a", "g f", "a d c", "a b c x a b c")
str_extract_all(x, "[a-z]", simplify = TRUE) # gives you multiple occurrences
# [a-z]: all letters from a to z
str_extract_all(x, "[a-z]", simplify = FALSE) %>% 
  unlist()





# 3.3 Grouped matched
# parentheses to extract parts of a complex match
# Word: a sequence of at least one character that isn’t a space: ([^ ]+)
# This definition does not work for all languages:
# https://en.wikipedia.org/wiki/Category:Writing_systems_without_word_boundaries
# Relevant if you
# ... (a) work on/in/with East and Southeast Asia, or Tibet;
# ... (b) are a historian or an archeologist.

noun <- "(A|a|The|the) ([^ ]+)" 
# ([^ ]+) is our definition of a word here !!
# Remember: + = {1,}
# ?? how is this insufficient for catching all nouns???

# extract sentences
has_noun <- sentences %>%
  str_subset(noun)# %>%
  #head(10)

# complete match with str_extract()
has_noun %>% 
  str_extract(noun)


# individual component with str_match()
has_noun %>% 
  str_match(noun) # matrix
has_noun %>% 
  str_match_all(noun) # List

## Try again with better definition
noun <- "(A|a|The|the|my|your|his|her|its|large|blue|purple|yellow) ([^ ]+)"


# working with tibble --> easier with tidyr::extract()
tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), "(A|a|The|the|my|your|his|her|its|large|blue|purple|yellow) ([^ ]+)", 
    remove = FALSE
  )

tibble(sentence = sentences) %>% 
  tidyr::extract(
    sentence, c("article", "noun"), noun, 
    remove = FALSE
  )




# 3.4 Replace matches
# Usage scenario: data cleaning

# mit str_replace() & str_replace_all()
x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-") # the first vowel
str_replace_all(x, "[aeiou]", "-") # all vowels


# multiple replacements:
x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))


# use back-references to insert components of the match.
head(sentences, 5)
# ... to change the order of the words
sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)


# replace the vowels with a slash and backslash
x <- c("apple", "pear", "banana")
x<- str_replace(x, "[aeiou]", "/")
str_replace_all(x, "/", "\\\\") # 2 Backslashes instead of one
cat(str_replace_all(x, "/", "\\\\"))




# 3.5 Splitting
# Usage scenario: data cleaning
sentences %>%
  head(5) %>% 
  str_split(" ") # split at empty space


"a|b|c|d" %>% 
  str_split("\\|") %>% # split at operator
  .[[1]]


# return matrix with "simplify"
sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE)


# specify maximum number of pieces
fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n = 2, simplify = TRUE)
sentences %>% str_split(" ", n = 5, simplify = TRUE) %>% head()


# Instead of splitting up strings by patterns, 
# you can also split up by character, line, sentence and word boundary()s:
?boundary() # Match boundaries between things.
x <- "This is a sentence.  This is another sentence."
str_view(1, boundary("word"))
str_split(x, " ")[[1]]
head(str_split(sentences, boundary("word")))

# How about a writing system without word boundaries?
y <- "这是一句话。然后高山流水这个成语又是一句话，类似于邯郸学步，但是这句话稍微复杂一点。。。这套软件件并不是乱七八糟的啊！"
str_split(y, " ")[[1]] # split along space does not work
str_split(y, boundary("word"))[[1]] # boundary("word") works for Chinese
# It can differentiate between words with 1, 2 or 4 characters ...
# there are but some minor inconsistencies with measure words, and Chinese points and commas are missing.



# 3.6 find matches
# str_locate() and str_locate_all() give you the starting and ending positions of each match.
# These are particularly useful when none of the other functions does exactly what you want. 
# You can use str_locate() to find the matching pattern, 
# str_sub() to extract and/or modify them.
?str_locate()
x <- c("apple", "pear", "banana")
str_locate(x, "a")
str_locate_all(x, "a") # output is a list


?str_sub()
# Extract a substring of length 3 from each word
str_sub(x, start = 2, end = 4)




# 4. Other types of pattern
# When you use a pattern that is a string, 
# it’s automatically wrapped into a call to regex():
# The regular call:
str_view(fruit, "nana")
# Is shorthand for
str_view(fruit, regex("nana"))


# Arguments for regex()
# ignore_case == TRUE
bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))
## Note: not all languages have capitalization ...


# multiline: allow ^ and $ to match the start and end of each line, rather than of the string
x <- "Line 1\nLine 2\nLine 3"
x
writeLines(x)  # \n: new line; you probably saw it before somewhere ...
str_extract_all(x, "^Line")[[1]] # get the first and only list item
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]

# comments: make complex RegEx more understandable
## more phone numbers ...
phone <- regex("
  \\(?     # optional opening parenthesis
  (\\d{3}) # area code
  [) -]?   # optional closing parenthesis, space, or dash
  (\\d{3}) # another three numbers
  [ -]?    # optional space or dash
  (\\d{3,}) # three more numbers
  ", comments = TRUE)

str_match("514-791-8141", phone)


# Outlook alternative functions:
## I largely skip this step, consult the book if you think it may be helpfull
?fixed() # faster alternative to regex, but works best with English (and presumably Italian)
?coll() # good for case-sensitive matching (locale argument), but slow ...


# other uses of regular expressions
# apropos() searches all objects available from the global environment. 
apropos("str_") # list all the string functions 

# dir() lists all the files in a directory.
getwd()
file.choose()
setwd("/Users/arminmuller/Github_repositories/R/")
head(dir(pattern = "\\.*$"))








