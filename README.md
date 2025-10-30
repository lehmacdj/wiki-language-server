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

MVP features (this will be a moving target, and mostly is a smaller list to keep
me focused on what I'm working on/thinking about right now):
- Note creation / actions to create notes from visual selections (i.e. completely replace neuron.vim)

Aspires to eventually support at least:
- Vim Plugin Features:
  - telescope extension for searching notes / backlinks of notes.
    - mostly implemented via commands in the language server
  - like conceal bits of syntax that are unnecessary to see most of the time
    - I've implemented concealment using ordinary syntax rules in my vim config; using LSP to handle what I want to do is slightly overkill short term
    - e.g. stuff to the left of the `|` in a `[[asdf|alt link text]]` link.
    - This can use [Semantic Tokens](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens)
      - use a custom `hidden` semantic token modifier that indicates that a symbol should be hidden
    - Maybe also doable with [inlay hints](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint), though this would bend what that's supposed to do
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
- Nothing yet!
