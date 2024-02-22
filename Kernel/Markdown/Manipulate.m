PrivateFunction[manipulateBoxesToMarkdown]

manipulateBoxesToMarkdown[tb:TagBox[_, Manipulate`InterpretManipulate[1]]] := Scope[
  anim = ManipulateAnimation @ Eval @ ToExpression @ tb;
  video = anim["Video"];
  videoBoxToMarkdown @ ToBoxes @ video
];