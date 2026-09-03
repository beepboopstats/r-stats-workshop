# Module 1 — pedagogical improvements

Working notes for a future session. Source of the review: a full read of
`content/module1.qmd` (744 lines) on **2026-09-02**, eight days before the
Sep 10 2026 session.

Nothing here has been implemented. Line numbers are "as of 2026-09-02" and will
drift — the **chunk labels** (`#| label: ...`) and section headings are the
stable anchors, so locate by those.

**Verdict going in:** the module is well-built. Sequencing is sound, prose is
strong, and the two deep-dive pages keep scope discipline honest. Every item
below is about *how learners practise*, not about what is covered.

---

## Decided — do not re-raise

Both were reviewed on 2026-09-02 and deliberately left alone:

- The **"Page Under Development"** callout at the top of `module1.qmd` stays.
- The **subtitle `"Biplabendu [ Billu ] Das"`** stays, even though Modules 2–4
  use the session date instead. The title's `:` separator (vs `·` on siblings)
  also stays.

---

## Additive — safe to do in one pass, no restructuring

### 1. Show the errors instead of describing them

**Highest payoff per unit of effort on this list.**

The page names the three canonical beginner errors but never displays one:

| Error | Named at | Currently |
|:--|:--|:--|
| `could not find function "filter"` | line 162, prose | never shown |
| `=` used where `==` is meant | line ~372, prose | never shown |
| `\|>` used where `+` belongs between ggplot layers | line ~592, called "the most common ggplot2 error there is" | never shown |

A novice therefore has no idea what an error looks like on screen, when it
appears, or which line R blames. Reading error messages is the most-used
practical skill a beginner has, and it is the only skill this module never
practises.

`content/module1-data-types.qmd:75` already uses `#| error: true`; Module 1 uses
it nowhere. Add deliberate-failure chunks, e.g. after the `fig-histogram` chunk:

````
```{r}
#| label: err-ggplot-pipe
#| error: true
# The most common ggplot2 error there is: |> between layers
abcd |>
  ggplot(aes(x = cognition_score)) |>
  geom_histogram()
```
````

Do the same for `=` vs `==` (near the `filter` chunk) and for a missing
`library()` call (near the `libraries` chunk — note this one needs care, since
the packages are already loaded by then; consider showing a captured message
rather than a live failure).

Global `execute` in `_quarto.yml` does not set `error`, so it defaults to
halting — per-chunk `#| error: true` is required and is the only thing that
makes these render.

### 2. Ask for a prediction before revealing output

`scripts/module1-live.R:67` does this once
(`# TODO: predict what class(c(TRUE, 5)) gives`). The page never does.

Four places where predict-then-run beats read-then-nod:

- the `coercion` chunk — will `class(mixed)` give `"numeric"`, `"character"`, or an error?
- the `na-trap` chunk — what does `mean(abcd$cognition_score)` print?
- the `fig-line` chunk — what happens if you drop `group = 1`?
- the `pipe-simple` chunk — what does the pipe hand to `mean()`?

Shape to use:

```
::: {.callout-note}
## Predict first
What will `class(mixed)` print — `"numeric"`, `"character"`, or an error?
Decide, then run it.
:::
```

### 3. `na.rm = TRUE` is unexplained magic for two sections

First appears in the `nested` chunk (line 328), then runs through every
`summarise()` in section 7 — but is not explained until section 8, line ~510,
where the page admits it: *"This is why `na.rm = TRUE` has been scattered
through the code above."*

For two sections learners copy an incantation they cannot parse, which is
exactly what makes R feel arbitrary to beginners.

Cheapest fix, at line 328: *"`na.rm = TRUE` means 'skip the missing values'.
Section 8 explains why they are there."* Reordering the sections would also work
but costs far more.

### 4. The pipe's first example undercuts the pipe

The `pipe-simple` chunk (line ~336) is:

```r
abcd$cognition_score |> mean(na.rm = TRUE)
```

Two problems. It uses `$`, which line 143 told them tidyverse style avoids. And
it pipes a **vector**, when the whole tidyverse model is table-in / table-out.
First examples anchor mental models.

Lead instead with `abcd |> filter(visit == "baseline")` and keep the `$` form as
the "you will also see this" aside.

### 5. Cheap wins

- **Minute markers per section.** Ten sections, two hours, no pacing signal for
  the instructor or a self-paced reader. E.g.
  `## 7. The five verbs that do the work · ~25 min`.
- **A checkpoint line after each major section** — "you should be able to run X
  and get Y; if not, say so in chat." Gives a live signal that the room is
  keeping up, and a stop-and-fix trigger for self-paced readers.
- **Open with the destination.** The `full-pipeline` chunk under "Putting it
  together" is a strong payoff that arrives cold at the end. Show it in the
  intro as *"this is what you will write by 2pm"* so every section is visibly a
  step toward it.

---

## Needs a decision before starting

### 6. All five exercises are the same task

Exercises 1.1–1.5 (`ex-1-1` … `ex-1-5`) are all blank-page pipeline authoring —
the highest-load activity available, and the only one on offer. Two additions
would flatten the ladder without lowering the ceiling:

- **A Parsons problem** for the pipe: give the four lines of the `ex-1-3`
  solution scrambled and ask for the order. Isolates *sequence* from *syntax*,
  which is precisely what the pipe is about, at far lower load than composing
  from nothing.
- **A debug-this**: hand over broken code plus the literal error message and ask
  what is wrong. Pairs directly with item 1.

Also worth signalling difficulty, and adding a stretch variant to 1.3 and 1.5 —
in a mixed room, fast finishers currently idle.

Open question: whether the live skeleton `scripts/module1-live.R` should carry
matching slots for the new exercise types. Its sections currently mirror the
page's 1–10 exactly, and that alignment is worth preserving.

### 7. Real data arrives too late — biggest win, biggest cost

Sections 1–3 spend ~150 lines on decontextualised notation (`sqrt(16)`,
`c(104, 98, 116, 91)`, `class(10.5)`) before the dataset appears in section 4 at
the `read-data` chunk. Exercise 1.1 is on an invented `sleep` vector. For
research staff who came because they have data, that is a long time before
anything touches the job.

Proposal: load `abcd` immediately after section 1, then teach vectors, `$` and
types **on real columns** — `abcd$age_months`, `class(abcd$visit)`,
`mean(abcd$sleep_hrs)`.

Costs a restructure of a third of the page, and `scripts/module1-live.R` would
have to move in lockstep. But it buys context for everything after it, and it
fits `CLAUDE.md`'s own rule better: base R notation "introduced only as much as
is needed to *read* code found elsewhere" is an argument for teaching it in
context, not as a standalone unit.

---

## Suggested order of work

1. Items 1, 2, 3, 4 and the cheap wins in item 5 — additive, non-disruptive,
   can all land in one pass.
2. Decide on item 6, then implement with the live script updated alongside.
3. Decide on item 7 last; it invalidates line references in everything above.

After any change: `quarto render` must exit 0 — with `echo: true` set globally,
a clean render is the only test this repo has, and `#| error: true` chunks are
expected to render their error text rather than halt.
