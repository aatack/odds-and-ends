import pyautogui as pg
from time import sleep
from random import uniform


def perform_group(group: [str]) -> ():
    if isinstance(group, str):
        group = [c for c in group]
    for key in group:
        pg.press(key)
        sleep(uniform(0.01, 0.03))


def perform_groups(groups: [[str]]) -> ():
    for group in groups:
        perform_group(group)
        sleep(uniform(0.3, 0.8))


def search(text):
    pg.hotkey("ctrl", "f")
    pg.typewrite(text, interval=uniform(0.01, 0.03))
    pg.press("esc")


perform_groups(
    [
        ["win", "c", "h", "r", "o", "m", "e", "enter"],
        "https://www.just-eat.co.uk/area/postcode-area",
        ["enter"],
    ]
)
sleep(3)
search("Restaurant")
pg.press("enter")
