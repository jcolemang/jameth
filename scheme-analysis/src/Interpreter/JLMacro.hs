
module JLMacro where

-- The argument to a macro transformer is a syntax object

-- Syntax objects are scheme values. Try running `(display (syntax (+ 1 2 3)))`

-- `syntax` creates a syntax object

-- A syntax object contains contextual information about an expression in
-- addition to its structure. This contextual information is used by the
-- expander to maintain hygiene and referential transparency

data SyntaxObjeect
  = X

-- start by just writing something which completely non hygenically expands a
-- mocro. After that, start adding in hygenic macro expansion

-- To do this, I have to first be able to determine the lexical nature of the
-- code.
