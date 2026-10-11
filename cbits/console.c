#include <windows.h>

/* These are in the headers of Windows 10 and later, and not in all of the
   ones this may be built with. */
#ifndef ENABLE_VIRTUAL_TERMINAL_INPUT
#define ENABLE_VIRTUAL_TERMINAL_INPUT 0x0200
#endif
#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif

static HANDLE input(void) { return GetStdHandle(STD_INPUT_HANDLE); }
static HANDLE output(void) { return GetStdHandle(STD_OUTPUT_HANDLE); }

/* Whether standard input, or standard output, is a console. */
int lambline_is_console(int out) {
  DWORD mode;
  return GetConsoleMode(out ? output() : input(), &mode) != 0;
}

/* Whether the console obeys the control sequences a terminal does. It is
   asked to, and then put back as it was. */
int lambline_speaks(void) {
  DWORD mode;
  if (!GetConsoleMode(output(), &mode)) return 0;
  if (!SetConsoleMode(output(), mode | ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) return 0;
  SetConsoleMode(output(), mode);
  return 1;
}

/* The size of the console's window, or 80 by 24 if there is none. */
int lambline_columns(void) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(output(), &info)) return 80;
  int width = info.srWindow.Right - info.srWindow.Left + 1;
  return width > 0 ? width : 80;
}

int lambline_rows(void) {
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(output(), &info)) return 24;
  int height = info.srWindow.Bottom - info.srWindow.Top + 1;
  return height > 0 ? height : 24;
}

/* Have keys come as they are typed, unechoed, as the sequences a terminal
   sends, and have the sequences that are written obeyed. Text goes both
   ways as UTF-8. How things were is put in four words, to be given back to
   lambline_restore. The answer is zero if it could not be done. */
int lambline_raw(int signals, unsigned int *saved) {
  DWORD in, out;
  if (!GetConsoleMode(input(), &in) || !GetConsoleMode(output(), &out)) return 0;
  saved[0] = in;
  saved[1] = out;
  saved[2] = GetConsoleCP();
  saved[3] = GetConsoleOutputCP();
  DWORD raw = in & ~(ENABLE_ECHO_INPUT | ENABLE_LINE_INPUT | ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT);
  if (!signals) raw &= ~ENABLE_PROCESSED_INPUT;
  raw |= ENABLE_VIRTUAL_TERMINAL_INPUT;
  if (!SetConsoleMode(input(), raw)) return 0;
  if (!SetConsoleMode(output(), out | ENABLE_PROCESSED_OUTPUT | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
    SetConsoleMode(input(), in);
    return 0;
  }
  SetConsoleCP(CP_UTF8);
  SetConsoleOutputCP(CP_UTF8);
  return 1;
}

void lambline_restore(const unsigned int *saved) {
  SetConsoleMode(input(), saved[0]);
  SetConsoleMode(output(), saved[1]);
  SetConsoleCP(saved[2]);
  SetConsoleOutputCP(saved[3]);
}
