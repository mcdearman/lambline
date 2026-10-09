#include <sys/ioctl.h>

/* The size of the terminal on a file descriptor, or 80 by 24 if it has none. */
int lambline_columns(int fd) {
  struct winsize ws;
  if (ioctl(fd, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) return 80;
  return ws.ws_col;
}

int lambline_rows(int fd) {
  struct winsize ws;
  if (ioctl(fd, TIOCGWINSZ, &ws) == -1 || ws.ws_row == 0) return 24;
  return ws.ws_row;
}
