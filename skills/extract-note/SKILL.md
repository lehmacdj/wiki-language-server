---
name: extract-note
description: >
  Extract a note from existing content. Searches for references to
  a topic, creates a new stub note, replaces inline references with
  wiki links, and populates the note with a short description.
user-invocable: true
---

Extract a wiki note from existing content for a given topic.

**Input**: A title or description of the topic (passed as the skill
argument). This may be an exact title like `Utsav Mamoria`, or
include disambiguating context like `Jane Doe the writer`. If the
argument includes extra context beyond a clean note title, decide
on an appropriate title (e.g. "Jane Doe (writer)" or just
"Jane Doe" if unambiguous) and confirm with the user if unclear.

The user may also provide full body content for the new note (e.g.
a bio or description to paste verbatim). When they do, include that
content verbatim in step 6, but still consider augmenting it with
additional context drawn from existing references (e.g. a short
lead-in summary, backlinks to related notes, or a subsection
grouping related mentions). The verbatim content is a floor, not a
ceiling. Steps 1-5 and 7 still apply.

**Process**:

### 1. Search for references

Search **exhaustively** for mentions of the topic across the wiki.
Use multiple search strategies:
- Exact phrase (e.g. "Utsav Mamoria")
- Individual significant words (e.g. "Utsav", "Mamoria")
- Known aliases, abbreviations, or alternate spellings if obvious
- Case-insensitive matching

Collect every file + line that references the topic. Read enough
surrounding context (a few lines around each match) to understand
how the topic is being referenced.

### 2. Check for existing notes (duplicate detection)

Run `wiki note --matching-title '<title>' --no-create-if-missing`
(with sandbox disabled -- needed for the global note title cache)
to see if any notes with similar titles already exist.

For each fuzzy match returned, read the note's title and first few
lines to determine whether it is actually about the same subject.
If a true duplicate exists, tell the user and stop -- do not create
a second note.

### 3. Assess ambiguity

There are two kinds of ambiguity to watch for:

**Title ambiguity**: The title could refer to multiple distinct
things (e.g. "Mercury" could be the planet, the element, or the
band). If the fuzzy match results or the reference search surface
multiple distinct meanings, summarize what was found and ask the
user to clarify the scope/purpose of the new note before
proceeding.

**Reference ambiguity**: Some search hits may mention the same word
or phrase but refer to something different from the intended topic.
For each match, decide whether it genuinely refers to the topic of
the new note. If uncertain about specific references, include them
in the summary and ask the user which ones to link.

If everything is clear, proceed without asking.

### 4. Create the note

Run:
```
wiki note --new-with-title '<title>'
```
(with sandbox disabled)

This outputs the path of the newly created note (e.g.
`./aBcDeFgHiJkL.md`). Record the slug from the filename.

### 5. Replace references with wiki links

For each confirmed reference, replace the mention of the topic
with a wiki link:
- Prefer the exact note title as display text:
  `[[<slug>|<note title>]]<!--wls-->` — the `<!--wls-->` comment
  tells the wiki language server to keep the display text in sync
  with the note's actual title
- When the exact title reads awkwardly in context, use natural
  display text instead: `[[<slug>|<display text>]]` (without
  `<!--wls-->`) so it won't be auto-updated
- Only replace references that genuinely refer to this topic
  (per the ambiguity assessment in step 3)
- If the reference is inside a URL or already a wiki link, leave
  it alone — but consider inserting a reference nearby if the
  context would benefit from one
- If the same file mentions the topic multiple times, link the
  first or most prominent mention; subsequent mentions can be left
  as plain text (use judgment)

### 6. Populate the new note

Edit the created note to add:
1. A 1-3 sentence description immediately after the `# Title`
   heading. This should summarize what the thing *is* based solely
   on information already present in the wiki. Do not invent
   context.
2. Optionally, subsections for distinct categories of information
   found in existing references (e.g. `## Articles`, `## Projects`,
   `## Events`). Only create subsections when there are distinct
   groupings worth separating. Keep it short -- only include things
   you'd want to see when referencing the newly created note.
3. Use wiki links (`[[slug|title]]<!--wls-->`) and external links
   where appropriate within the note content.

The note should be **short** -- mostly a stub. The backlinks from
the references we just created provide the detailed context.

### 7. Summarize

Output a summary listing:
- The new note's path and title
- How many references were found and in which files
- Which references were replaced with wiki links
- Any references that were skipped and why
- Any ambiguities that were resolved (and how)
