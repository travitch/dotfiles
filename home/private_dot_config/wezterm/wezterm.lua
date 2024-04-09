local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.font = wezterm.font 'MonoLisa'
config.color_scheme = 'Solarized (light) (terminal.sexy)'

config.enable_tab_bar = false
config.term = 'wezterm'

return config
