PublicHead[MarkdownHeadingPattern, MarkdownNoteLinkPattern, MarkdownHyperlinkPattern, MarkdownInlineCodePattern, MarkdownBlockCodePattern, MarkdownEmphasisPattern]

$emphasisFragment = StringReplace["""(?:(?:\*Z\*)|(?:/Z/)|(?:(?<!\S)_Z_(?!\S)))""", "Z" -> """(?:(?:\S[^\n]*?\S)|\S)"""];

("/" ~~ LetterCharacter..)... ~~ WordBoundary

(* TODO: why aren't these using RawStrExp? *)
DefineStringPattern[
  MarkdownHeadingPattern      :> "^#{1,4} (?:[^\n])+$",
  MarkdownNoteLinkPattern     :> "\\[\\[□\\]\\]",
  MarkdownHyperlinkPattern    :> "\\[□\\]\\(http[^ \n]+\\)",
  MarkdownInlineCodePattern   :> "`□`",
  MarkdownBlockCodePattern    :> "^```[^`]+\n```",
  MarkdownEmphasisPattern     :> $emphasisFragment
];

(**************************************************************************************************)

PublicHead[MarkdownTagPattern, MarkdownSubtagPattern, MarkdownDelimitedTagPattern]

DefineStringPattern[
  MarkdownTagPattern          :> """(?<![[:alnum:]/])#\w+(?:\/\w+)*(?![[:alnum:]\]])""",
  MarkdownSubtagPattern[tag_] :> RawStrExp["""(?<![[:alnum:]/])#""", EvalStrExp @ tag, """(?:\/\w+)*(?![[:alnum:]\]])"""],
  MarkdownDelimitedTagPattern :> """(?<![[:alnum:]/])#[[:alnum:] ]+(?:\/[[:alnum:] ]+)*#(?![[:alnum:]\]])"""
]
