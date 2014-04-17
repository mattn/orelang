if exists("b:current_syntax")
  finish
endif

syn case match

syn keyword     oreDeclaration       var

hi def link     oreDirective         Statement
hi def link     oreDeclaration       Type

syn keyword     oreStatement         return break continue throw
syn keyword     oreConditional       if else switch try catch finally
syn keyword     oreLabel             case default
syn keyword     oreRepeat            for while

hi def link     oreStatement         Statement
hi def link     oreConditional       Conditional
hi def link     oreLabel             Label
hi def link     oreRepeat            Repeat

syn match       oreDeclaration       /\<func\>/
syn match       oreDeclaration       /^func\>/

syn keyword     oreBuiltins          keys len
syn keyword     oreBuiltins          println printf print
syn keyword     oreConstants         true false nil

hi def link     oreBuiltins          Keyword
hi def link     oreConstants         Keyword

" Comments; their contents
syn keyword     oreTodo              contained TODO FIXME XXX BUG
syn cluster     oreCommentGroup      contains=oreTodo
syn region      oreComment           start="#" end="$" contains=@oreCommentGroup,@Spell

hi def link     oreComment           Comment
hi def link     oreTodo              Todo

" ore escapes
syn match       oreEscapeOctal       display contained "\\[0-7]\{3}"
syn match       oreEscapeC           display contained +\\[abfnrtv\\'"]+
syn match       oreEscapeX           display contained "\\x\x\{2}"
syn match       oreEscapeU           display contained "\\u\x\{4}"
syn match       oreEscapeBigU        display contained "\\U\x\{8}"
syn match       oreEscapeError       display contained +\\[^0-7xuUabfnrtv\\'"]+

hi def link     oreEscapeOctal       oreSpecialString
hi def link     oreEscapeC           oreSpecialString
hi def link     oreEscapeX           oreSpecialString
hi def link     oreEscapeU           oreSpecialString
hi def link     oreEscapeBigU        oreSpecialString
hi def link     oreSpecialString     Special
hi def link     oreEscapeError       Error

" Strings and their contents
syn cluster     oreStringGroup       contains=oreEscapeOctal,oreEscapeC,oreEscapeX,oreEscapeU,oreEscapeBigU,oreEscapeError
syn region      oreString            start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@oreStringGroup
syn region      oreRawString         start=+`+ end=+`+

hi def link     oreString            String
hi def link     oreRawString         String

" Characters; their contents
syn cluster     oreCharacterGroup    contains=oreEscapeOctal,oreEscapeC,oreEscapeX,oreEscapeU,oreEscapeBigU
syn region      oreCharacter         start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=@oreCharacterGroup

hi def link     oreCharacter         Character

" Regions
syn region      oreBlock             start="{" end="}" transparent fold
syn region      oreParen             start='(' end=')' transparent

" Integers
syn match       oreDecimalInt        "\<\d\+\([Ee]\d\+\)\?\>"
syn match       oreHexadecimalInt    "\<0x\x\+\>"
syn match       oreOctalInt          "\<0\o\+\>"
syn match       oreOctalError        "\<0\o*[89]\d*\>"

hi def link     oreDecimalInt        Integer
hi def link     oreHexadecimalInt    Integer
hi def link     oreOctalInt          Integer
hi def link     Integer             Number

" Floating point
syn match       oreFloat             "\<\d\+\.\d*\([Ee][-+]\d\+\)\?\>"
syn match       oreFloat             "\<\.\d\+\([Ee][-+]\d\+\)\?\>"
syn match       oreFloat             "\<\d\+[Ee][-+]\d\+\>"

hi def link     oreFloat             Float
hi def link     oreImaginary         Number

syn sync minlines=500

let b:current_syntax = "ore"
