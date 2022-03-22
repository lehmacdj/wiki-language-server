# wiki-lsp
Language server protocol for markdown notes with wiki style links.

Currently supports:
- parses documents and reports diagnostic if parsing failed. IIRC markdown never
  fails to parse so this is pretty much a no-op though.
- goto definition that implements jumping between notes via links

MVP features (this will be a moving target, and mostly is a smaller list to keep
me focused on what I'm working on right now):

Technical concerns, that probably will become relevant when implementing below:
- Need some global state synchronization like Reactor pattern from Reactor.hs
  in lsp package example

Aspires to support at least:
- autocomplete for note titles when starting a new link
- automatically transclude note titles (defined by title in yaml front matter or
  otherwise first `<h1>` of the note) into wiki links (e.g. `[[asdf]]` can be
  turned into `[[asdf|Note title]]`), this should probably be implemented by
  means of formatting operations.
Stretch goals (roughly ordered by priority):
- Optional vim plugin, that does stuff like:
  - like conceal bits of syntax that are unnecessary to see most of the time
    - e.g. stuff to the left of the `|` in a `[[asdf|alt link text]]` link.
  - telescope extension for searching notes / backlinks of notes.
  - other vim specific stuff if applicable
- autoformatter ala gofmt/ormolu that standardizes line lengths to less than 80
  - compatibility with prettier, in the sense that prettier doesn't change code
    formatted by my formatter would be nice (though this is fairly low priority,
    especially I don't want to have to make changes to keep in sync with
    prettier).
- Hover shows a preview of the note under the link or notes containing that word
  if not a link
- integration with markdownlint or some other linting tool for markdown to catch
  syntax problems that would lead to weird rendering; potentially even an
  implementation from scratch of such a tool based on pandoc commonmark parsers
- obsidian extension for transcluding note titles in places where titled wiki
  links aren't sufficient (e.g. the graph view / list of notes in the toolbar)
- accurate syntax highlighting via lsp symbols
  - potentially possible to even do embedded code blocks using `skylighting`
    library or similar, which would be pretty cool

Won't implement:
- Nothing yet!
