# wiki-lsp
Language server protocol for markdown notes with wiki style links.

Currently supports:
- Nothing so far

MVP features (this will be a moving target, and mostly is a smaller list to keep
me focused on what I'm working on right now):
- parse documents and report diagnostic if parsing failed

Aspires to support at least:
- goto definition that implements jumping between notes via links
- autocomplete for note titles when starting a new link
- automatically transclude note titles (defined by title in yaml front matter or
  otherwise first `<h1>` of the note) into wiki links (e.g. `[[asdf]]` can be
  turned into `[[asdf|Note title]]`), this should probably be implemented by
  means of formatting operations.

Stretch goals (roughly ordered by priority):
- autoformatter ala gofmt/ormolu that standardizes line lengths to less than 80
- integration with markdownlint or some other linting tool for markdown to catch
  syntax problems that would lead to weird rendering; potentially even an
  implementation from scratch of such a tool based on pandoc commonmark parsers
- Optional vim plugin, that does stuff like conceal bits of syntax that are
  unnecessary to see most of the time. An example of the kind of syntax to
  conceal is the stuff to the left of the `|` in a `[[asdf|alt link text]]`
  link.
- accurate syntax highlighting via lsp symbols

Won't implement:
- Nothing yet!
