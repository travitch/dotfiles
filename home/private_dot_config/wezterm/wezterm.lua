local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.font = wezterm.font_with_fallback { 'MonoLisa', 'Noto Color Emoji' }
-- config.color_scheme = 'Builtin Solarized Light'
-- config.color_scheme = 'Violet Light'
-- config.color_scheme = 'Alabaster'
config.color_scheme = 'Night Owlish Light'
-- config.color_scheme = 'ayu_light'
-- config.color_scheme = 'dayfox'
-- config.color_scheme = 'Solarized Light (Gogh)'
-- config.color_scheme = 'Modus-Operandi'

config.enable_tab_bar = false
config.term = 'wezterm'
config.adjust_window_size_when_changing_font_size = false
-- config.default_cursor_style = 'SteadyBar'

return config
