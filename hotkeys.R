# hotkeys.R

### Modify as necessary
### Map Tab Hotkeys
HOTKEY_TIME_UP_RANGE <- "c" # move slider to the right by the width of the current selection
HOTKEY_TIME_DOWN_RANGE <- "x" # move slider to the left by the width of the current selection
HOTKEY_TIME_UP_STEP <- "C" # move slider to the right by HOTKEY_TIME_STEP
HOTKEY_TIME_DOWN_STEP <- "X" # move slider to the left by HOTKEY_TIME_STEP

HOTKEY_TIME_STEP <- 30 # step size in seconds for HOTKEY_TIME_UP_STEP and HOTKEY_TIME_DOWN_STEP

HOTKEY_TOGGLE_CONES <- "q" # toggle antenna cones on&off in map
HOTKEY_TOGGLE_BEARINGS <- "b" # toggle bearings on&off in map

HOTKEY_DEL_MAN_POINT <- "d"
###




hotkeys<-c(HOTKEY_TIME_UP_RANGE, HOTKEY_TIME_DOWN_RANGE, HOTKEY_TIME_UP_STEP, HOTKEY_TIME_DOWN_STEP, HOTKEY_TOGGLE_CONES, HOTKEY_TOGGLE_BEARINGS, HOTKEY_DEL_MAN_POINT)
