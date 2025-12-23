# Wiki Language Server
This is a Language Server Protocol server that implements utilities for markdown documents in a wiki like structure.

## Architecture
- Handlers (in src/Handlers/) implement LSP requests
- Markdown is parsed using Pandoc, we transform the Pandoc AST for formatting etc.
- MyPrelude re-exports ClassyPrelude and useful helper functions. Prefer generally useful functions somewhere within the `MyPrelude.*` module hierarchy

## Testing
- Prefer writing tests in the file with with the function being tested
- Use Hspec, it's imported by default via MyPrelude
- Use the `[md| ... |]` quasi-quoter to write tests that use the pandoc AST
