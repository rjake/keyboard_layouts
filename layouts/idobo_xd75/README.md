# Condensed Idobo XD75 layout

Setup keyboard to be used when OS is using QWERTY. Otherwise, install [Workman layout](https://workmanlayout.org)

![](https://github.com/rjake/keyboard_layouts/raw/master/layouts/idobo_xd75/condensed_layout/layers.png)


# Full Idobo XD75 layout

Setup keyboard like QWERTY + install [Workman layout](https://workmanlayout.org) on PC

### Layer 0 - QWERTY

![](https://github.com/rjake/keyboard_layouts/raw/master/layouts/idobo_xd75/full_layout/layer_00.png)

# Default(ish)
![](https://github.com/rjake/keyboard_layouts/raw/master/layouts/idobo_xd75/default_layout/layer_00.png)

# Steps to build
1. go to https://config.qmk.fm/#/idobo/LAYOUT_ortho_5x15
2. `[UPLOAD KEYMAP.JSON]` to import `.json` file of layers
3. Edit layers
4. Export
    * `[COMPILE]`
    * `[FIRMWARE]`
    * `[DOWNLOAD KEYMAP.JSON]`
5. Document
    * Run [visualize.R](https://github.com/rjake/keyboard_layouts/blob/master/analysis/visualize.R)
