(**************************************************************************************************)

PackageExport["PetriNet"]

PetriNet[rules_] := Scope[
  constructRewritingSystem["PetriNet", rules]
]
