# wiki-lsp
Language server protocol for markdown notes with wiki style links.

Currently supports:
- parses documents and reports diagnostic if parsing failed.
  - IIRC markdown never fails to parse so this is pretty much a no-op though.
- goto definition that implements jumping between notes via links
- transclusion of note titles into markdown on save (via formatting in LSP)
- caches for title information derived from the first `# H1` in notes
- `textDocument/completion` requests with:
  - fuzzy matching based on title cache
  - resolve request for preview of file

Most actively interested in:
- commands for creating notes:
  - expose as LSP commands & bind in an optional vim plugin
  - replace `\n` + `\d` macros that I currently use
  - allow more flexible/context dependent note creation, i.e. transforming a single bullet point + sublist into note title + bullets at top level
- create notes from completion menu
  - `[[redistribution` should show a completion option that creates a new note and completes a link if one doesn't exist
- title transclusion improvements: auto-capitalize lowercased titles when at beginning of sentence
  - e.g. `In one dimension they're absolutely correct. [[fbHxiy10Ib2g|Redistribution]]<!--wls--> necessarily...` even if the note is titled `# redistribution`
- completion for tags in the frontmatter
- automatic renumbering for lists, e.g. `1. ...\n3. ...\n` turns into `1. ...\n2. ...\n`

Aspires to eventually support:
- Vim Plugin Features:
  - telescope extension for searching notes / backlinks of notes.
    - mostly implementable via commands in the language server, just a tiny amount of telescope glue probably
- warnings for misleading/likely to be incorrect markup
  - integration with markdownlint or some other linting tool for markdown to catch syntax problems that would lead to weird rendering; potentially even an implementation from scratch of such a tool based on pandoc commonmark parsers
- more autocomplete
  - autocomplete for tags (when editing in YAML frontmatter)
  - autocomplete for GitHub style id link targets (e.g. a heading titled "Wiki Language Server" turns into `#wiki-language-server`)

Technical concerns, that might eventually become relevant:
- I never see practical problems with performance currently but if I do I'd need to add:
  - ability to handle concurrent requests / state synchronization
  - this might need something like the reactor pattern which is used in HLS + one of the example LSPs in the lsp package
  - I think it likely doesn't though, as long as only extremely responsive things run on the main event handling thread, things should be fine I think

Stretch goals (roughly ordered by priority, these are things that would be nice to have, but I'm not even sure I'll ever be motivated enough to implement):
- Code actions for checking/unchecking checkboxes (i.e. `- [ ]` to `- [x]` and vice versa)
- autoformatter ala gofmt/ormolu that standardizes line lengths to less than 80
  - compatibility with prettier, in the sense that prettier doesn't change code formatted by my formatter would be nice (though this is fairly low priority, especially I don't want to have to make changes to keep in sync with prettier).
- Hover shows a preview of the note under the link or notes containing that word if not a link
- obsidian extension for transcluding note titles in places where titled wiki links aren't sufficient (e.g. the graph view / list of notes in the toolbar)
- accurate syntax highlighting via lsp symbols
  - potentially possible to even do embedded code blocks using `skylighting` library or similar, which would be pretty cool

Won't implement:
- conceal unnecessary markdown syntax
  - it's much easier to just implement this with treesitter queries in neovim directly
  - We could use [Semantic Tokens](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens) to provide this functionality to other clients
    - use a custom `hidden` semantic token modifier that indicates that a symbol should be hidden
  - Alternatively [inlay hints](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint) are an LSP "conceal" feature, but I'm not sure they can be used to completely hide stuff, maybe only reduce to a single character
