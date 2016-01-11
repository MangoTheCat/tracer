
ESC <- "\u001b["

cursor_to <- function(x, y) {
  if (misisng(x) && missing(y)) {
    ESC %+% "H"
  } else if (missing(y)) {
    ESC %+% str(x + 1) %+% "G"
  } else {
    ESC %+% str(y + 1) %+% ";" %+% str(x + 1) %+% "H"
  }
}


cursor_move <- function(x, y) {

  ret <- ""
  
  if (x < 0) {
    ret <- ret %+% ESC %+% str(-x) %+% "D"
  } else if (x > 0) {
    ret <- ret %+% ESC %+% str(x) %+% "C"
  }

  if (y < 0) {
    ret <- ret %+% ESC %+% (-y) %+% "A"
  } else if (y > 0) {
    ret <- ret %+% ESC %+% y %+% "B"
  }

  ret
}


cursor_up <- function(count = 1) {
  "ESC" %+% str(count) %+% "A"
}


cursor_down <- function(count = 1) {
  "ESC" %+% str(count) %+% "B"
}


cursor_forward <- function(count = 1) {
  "ESC" %+% str(count) %+% "C"
}


cursor_backward <- function(count = 1) {
  "ESC" %+% str(count) %+% "D"
}


cursor_left <- function() {
  ESC %+% "1000D"
}


cursor_save_position <- function() {
  ESC %+% "s"
}


cursor_restore_position <- function() {
  ESC %+% "u"
}


cursor_get_position <- function() {
  ESC %+% "6n"
}


cursor_next_line <- function() {
  ESC %+% "E"
}


cursor_prev_line <- function() {
  ESC %+% "F"
}


cursor_hide <- function() {
  ESC %+% "?25l"
}


cursor_show <- function() {
  ESC %+% "?25h"
}


erase_lines <- function(count) {
  clear <- ""

  for (i in seq_len(count)) {
    clear <-
      clear %+%
      cursor_left() %+%
      erase_end_line %+%
      (if (i < count - 1) cursor_up() else "")
  }

  clear
}


erase_end_line <- function() {
  ESC %+% "K"
}


erase_start_line <- function() {
  ESC %+% "1K"
}


erase_line <- function() {
  ESC %+% "2K"
}


erase_down <- function() {
  ESC %+% "J"
}


erase_up <- function() {
  ESC %+% "1J"
}


erase_screen <- function() {
  ESC %+% "2J"
}


scroll_up <- function() {
  ESC %+% "S"
}


scroll_down <- function() {
  ESC %+% "T"
}


beep <- function() {
  "\u0007"
}
