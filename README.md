# wiki-lsp
Language server protocol for markdown notes with wiki style links.

Currently supports:
- parses documents and reports diagnostic if parsing failed.
  - IIRC markdown never fails to parse so this is pretty much a no-op though.
- goto definition that implements jumping between notes via links
- transclusion of note titles into markdown on save (via formatting in LSP)

MVP features (this will be a moving target, and mostly is a smaller list to keep
me focused on what I'm working on/thinking about right now):
- cache for title information (it's slow when saving some files already; probably going to be too slow for autocomplete on startup without some kind of cache)
  - currently thinking just a fairly simple text file that serializes an in memory data structure because the amount of data should be small enough that that is viable. Might be a better idea to immediately go for sqlite or some other embedded database though, to avoid scaling problems as my number of notes grows
- autocomplete for note titles when starting a new link
- autocomplete for tags (when editing in YAML frontmatter)
- autocomplete for GitHub style id link targets (e.g. a heading titled "Wiki Language Server" turns into `#wiki-language-server`)
- A Vim Plugin that offers
  - telescope extension for searching notes / backlinks of notes.
    - mostly implemented via commands in the language server

Aspires to eventually support at least:
- Vim Plugin Features:
  - like conceal bits of syntax that are unnecessary to see most of the time
    - e.g. stuff to the left of the `|` in a `[[asdf|alt link text]]` link.
    - This can use [Semantic Tokens](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens)
      - use a custom `hidden` semantic token modifier that indicates that a symbol should be hidden
    - Maybe also doable with [inlay hints](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_inlayHint), though this would bend what that's supposed to do
- warnings for misleading/likely to be incorrect markup
  - integration with markdownlint or some other linting tool for markdown to catch syntax problems that would lead to weird rendering; potentially even an implementation from scratch of such a tool based on pandoc commonmark parsers

Technical concerns, that might eventually become relevant:
- Need some global state synchronization like Reactor pattern from Reactor.hs
  in lsp package example

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
