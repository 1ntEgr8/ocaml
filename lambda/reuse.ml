open Lambda

let ru expr = expr

let insert_reuse_tokens program =
  { program with code= ru program.code }

let ru_spec expr = expr

let specialize_reuses program =
  { program with code= ru_spec program.code }
