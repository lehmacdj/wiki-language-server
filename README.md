# wiki-lsp
Language server protocol for markdown notes with wiki style links.

Currently supports:
- Nothing so far

Aspires to support at least:
- goto definition that implements jumping between notes via links
- autocomplete for note titles when starting a new link
- automatically transclude note titles (defined by title in yaml front matter or
  otherwise first `<h1>` of the note) into wiki links (e.g. `[[asdf]]` can be
  turned into `[[asdf|Note title]]`).

Stretch goals:
- Optional vim plugin, that does stuff like conceal bits of syntax that are
  unnecessary to see most of the time. An example of the kind of syntax to
  conceal is the stuff to the left of the `|` in a `[[asdf|alt link text]]`
  link.

Won't implement:
- Nothing yet!
