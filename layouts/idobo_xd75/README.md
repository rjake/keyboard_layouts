# Condensed Idobo XD75 layout

Setup keyboard to be used when OS is using QWERTY. Otherwise, install [Workman layout](https://workmanlayout.org)

![](https://github.com/rjake/keyboard_layouts/raw/master/layouts/idobo_xd75/condensed_layout/layers.png)

# Steps to build
1. go to https://config.qmk.fm/#/idobo/LAYOUT_ortho_5x15
2. `[UPLOAD KEYMAP.JSON]` to import `.json` file of layers
3. Edit layers
4. Export
    * `[DOWNLOAD KEYMAP.JSON]`
    * `[COMPILE]` (takes a few seconds)
    * `[DOWNLOAD FIRMWARE]` - `.hex`
5. Open QMK Toolbox
   * Open `.hex` file
   * Select MCU "atmega32u4"
   * Press button in back of keyboard
   * Hit `[Flash]`
6. Document
    * Run [visualize.R](https://github.com/rjake/keyboard_layouts/blob/master/analysis/visualize.R)



# Full Idobo XD75 layout

![](https://github.com/rjake/keyboard_layouts/raw/master/layouts/idobo_xd75/full_layout/layer_00.png)

# Default(ish)
![](https://github.com/rjake/keyboard_layouts/raw/master/layouts/idobo_xd75/default_layout/layer_00.png)
