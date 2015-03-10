" Language:   ni lisp
" Maintainer: Spencer Tipping

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'ni'
endif

syn case match
set iskeyword=37,38,42-63,65-90,94-122,124,127-255

syn region niShebang start=/\%^#!/ end=/$/
syn region niList             matchgroup=niParens     start=/(/          end=/)/ contains=@niTop
syn region niVector           matchgroup=niParens     start=/\[/         end=/]/ contains=@niTop
syn region niMap              matchgroup=niParens     start=/{/          end=/}/ contains=@niTop
syn region niSoftString       matchgroup=niQuoteMarks start=/\(^\|\s\)"/ end=/"/ contains=niSoftEscape,@niStringInterpolable

syn cluster niTop add=niList,niVector,niMap,niSoftString,niHardString,niInterpolatedWord,niCore,niMeta,niProbablyMeta,niEscapedWord,niLineComment
syn cluster niInterpolable add=niList,niInterpolatedWord
syn cluster niStringInterpolable add=niInterpolatedList,niInterpolatedWord

syn keyword niCore gensym sym-str str-sym to-array to-hash to-list
syn keyword niCore aget type car cdr cons uncons list count = > not print
syn keyword niCore macroexpand eval apply macro-fn cps-convert nil get has?

syn match niSoftEscape /\\./     contained
syn match niHardEscape /\\[\\']/ contained

syn keyword niMeta fn\* co\* amb\* nth\*

syn match niFunctionName     /(\@<=\k\+/ containedin=niList
syn match niDefFunctionName  /\k\+/      contained
syn match niProbablyMeta     /(\@<=def\k*\s\+/ nextgroup=niDefFunctionName
syn match niInterpolatedWord /\(@!\|[!@$]\)\k\+/

syn match niLineComment /#.*/
syn match niEscapedWord /\^\+\k\+/

hi def link niShebang             Special
hi def link niCore                Keyword
hi def link niMeta                Type
hi def link niProbablyMeta        Type
hi def link niParens              Special
hi def link niQuoteMarks          Special
hi def link niSoftString          String
hi def link niInterpolatedWord    Identifier
hi def link niInterpolationParens Special
hi def link niLineComment         Comment
hi def link niEscapedWord         Special
hi def link niFunctionName        Identifier
hi def link niDefFunctionName     Identifier

hi def link niSoftEscape          Special
hi def link niHardEscape          Special
