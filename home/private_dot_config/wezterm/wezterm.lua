local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.font = wezterm.font 'MonoLisa'
-- config.color_scheme = 'Builtin Solarized Light'
config.color_scheme = 'dayfox'



config.enable_tab_bar = false
config.term = 'wezterm'
config.adjust_window_size_when_changing_font_size = false


return config
