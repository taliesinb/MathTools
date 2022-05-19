(**************************************************************************************************)

PackageExport["PetriNet"]

PetriNet[rules_] := Scope[
  constructRewritingSystsem["PetriNet", rules]
]
