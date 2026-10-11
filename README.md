# lambline

A line editor for Haskell programs: a port of
[rustyline](https://github.com/kkawakam/rustyline), written against its
source at version 18.

```haskell
import Lambline

main :: IO ()
main = do
  editor <- newEditor defaultConfig
  setHelper editor . Just $
    defaultHelper
      { complete = filenameCompleter,
        hint = historyHinter,
        highlight = matchingBracketHighlighter,
        validate = matchingBracketValidator
      }
  let loop =
        readline editor "> " >>= \case
          Right line -> addHistoryEntry editor line >> putStrLn line >> loop
          Left Interrupted -> loop
          Left Eof -> pure ()
  loop
```

`cabal run lambline-demo` runs something like that (`-- vi` for Vi mode,
`-- ticks` to watch another thread print while you type). `cabal test` runs
the tests. It needs only GHC and the libraries that ship with it, on a POSIX
system or on Windows 10 or later.

On Windows the console is asked to behave as a terminal does: to send keys
as the control sequences a terminal sends, and to obey the ones written to
it. Everything else is then the same code. Text is read and written as
UTF-8 while a line is being edited. Ctrl-D ends the input, as it does
elsewhere, and there is no Ctrl-Z to suspend with.

## What is ported

| | |
| --- | --- |
| Editing | Emacs and Vi modes with rustyline's key bindings, numeric arguments, a kill ring with yank-pop, undo, transposing, changing case, quoted insert, bracketed paste. |
| Vi | Insert, command and replace modes; operators `d` `c` `y` `<` `>` with motions and counts; `f` `t` `;` `,`; `.` to repeat; `p` `P`; `r` `R` `s` `S` `x` `X`; `u`. |
| Several lines | Input may span lines. Up and Down move between them, then through the history. |
| History | Previous and next, first and last, incremental search (Ctrl-R, Ctrl-S), search by prefix. Files in rustyline's format: load, save, append. |
| Completion | `complete` in a `Helper`. Shown one candidate per Tab (`Circular`) or completed as far as possible and then listed, paged, as in Bash (`List`). `filenameCompleter` is provided. |
| Hints | `hint` in a `Helper`, shown after the input and taken with Right. `historyHinter` is provided. |
| Highlighting | `highlight`, `highlightPrompt`, `highlightHint`, `highlightCandidate`. `matchingBracketHighlighter` is provided. |
| Validation | `validate` answers `Valid`, `Invalid` or `Incomplete` when Enter is pressed, each with an optional message. `matchingBracketValidator` is provided. |
| Bindings | `bindSequence` binds a key or a sequence of keys to a command, or to a function that picks one from the state of the editor. |
| Configuration | Everything in rustyline's `Config` that applies: history size and duplicates, completion type and prompt limit, edit mode, bell, colour, tab stop, indent size, escape timeout, bracketed paste, synchronized output, signals, checking the cursor position, adding to history automatically. |
| The rest | `readlineWithInitial`; an external printer, for output from other threads while the editor waits; Ctrl-Z; redrawing when the terminal is resized; plain line reading when there is no terminal, with the validator still joining lines. |

### Keys in Emacs mode

| Key | |
| --- | --- |
| Enter, Ctrl-J, Ctrl-M | Accept the input, or start a new line if the validator finds it incomplete |
| Ctrl-B, Ctrl-F, Left, Right | Move by character |
| Alt-B, Alt-F, Ctrl-Left, Ctrl-Right | Move by word |
| Ctrl-A, Ctrl-E, Home, End | Start and end of the line |
| Up, Down | Move by line, then through the history |
| Ctrl-P, Ctrl-N | Through the history |
| Alt-<, Alt-> | Oldest entry, and back to the input |
| Ctrl-R, Ctrl-S | Search the history as you type |
| Backspace, Ctrl-H, Delete, Ctrl-D | Delete a character |
| Ctrl-K, Ctrl-U | Kill to the end or start of the line |
| Ctrl-W, Alt-Backspace, Alt-D | Kill a word |
| Ctrl-Y, Alt-Y | Yank, and replace the yank with the kill before it |
| Ctrl-_, Ctrl-X Ctrl-U | Undo |
| Ctrl-T, Alt-T | Transpose characters, words |
| Alt-U, Alt-L, Alt-C | Uppercase, lowercase, capitalize a word |
| Tab, Shift-Tab | Complete |
| Right, at the end | Take the hint |
| Ctrl-V, Ctrl-Q | Insert the next character as it is |
| Ctrl-], Ctrl-Alt-] | Move to a character, forwards or backwards |
| Alt-digits, Alt-minus | A count for the next command |
| Ctrl-L | Clear the screen |
| Ctrl-G, Escape | Abandon a search or completion |
| Ctrl-C | Give up the input: `Left Interrupted` |
| Ctrl-D, on empty input | `Left Eof` |
| Ctrl-Z | Suspend |

As in rustyline, nothing is bound to "start a new line regardless". Bind it:

```haskell
bindSequence editor (KeySeq [alt '\r']) (Simple Newline)
```

## Where it differs

Two things are added:

- `continuationPrompt` in `Config` is shown at the start of each line of
  input after the first. rustyline starts those in the first column.
- `highlight` is told why the input is being redrawn, so that a highlighter
  can leave the accepted input unmarked without keeping state.

Some things are left out:

- Reading from `/dev/tty` instead of stdin (`Behavior`).
- Consoles that do not understand control sequences, which on Windows is
  those before Windows 10. On one of those lines are read plainly.
- Fuzzy completion, which rustyline does with skim.
- History in SQLite, and locking of history files.
- Grapheme clusters: each character is taken as one, and its width is
  estimated, so emoji sequences and some scripts will put the cursor in the
  wrong column.
- `Completer::update`, for replacing the text in some way other than
  dropping the candidate in.
- `highlight_char` and `validate_while_typing`. rustyline uses the first to
  decide when it can skip a redraw; lambline redraws on every key.

And a few behave differently:

- With no terminal, input the validator calls `Invalid` is discarded with
  its message and reading starts again; input left incomplete when stdin
  ends is returned rather than dropped.
- Vi's `>` and `<` indent the lines the movement covers. (rustyline looks
  for the end of that range from the wrong end.)
- With no `keyseqTimeout`, a lone Escape is not seen until the next key, as
  in rustyline. In Vi mode that is rarely what you want: set one.

## How it is put together

| Module | |
| --- | --- |
| `Lambline` | The editor, `readline`, history, bindings. |
| `Lambline.Config` | Preferences. |
| `Lambline.Helper` | Completion, hints, highlighting, validation, and the ones provided. |
| `Lambline.Keys` | Keys, and decoding terminal input into them. |
| `Lambline.Command` | Commands and movements. |
| `Lambline.Binding` | Custom bindings. |
| `Lambline.Engine` | The keymaps, and carrying out commands. |
| `Lambline.LineBuffer` | The text and cursor, and every operation on them. |
| `Lambline.Undo`, `Lambline.KillRing`, `Lambline.History` | What their names say. |
| `Lambline.Layout` | Where text lands on screen, and the redraw. |
| `Lambline.Terminal` | What the engine needs of a terminal, and the real one. |
| `Lambline.Console` | What differs between systems in driving one: `posix/` over termios, `windows/` over the console. Not exposed. |

The engine reaches the terminal only through a record of functions, so the
tests run whole editing sessions against a fake one: keys from a list, and
output played into a small terminal emulator to see what would be on screen.
