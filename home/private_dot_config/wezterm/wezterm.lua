local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.font = wezterm.font {
   family = 'Monaspace Neon NF',
   weight = 'Medium',
   harfbuzz_features = {'cv01=2', 'calt', 'liga', 'ss01', 'ss02', 'ss03', 'ss06', 'ss07', 'ss09', 'ss10'},
}

-- config.font = wezterm.font_with_fallback { 'MonoLisa', 'Noto Color Emoji' }
-- config.font = wezterm.font_with_fallback { 'Maple Mono NF', 'Noto Color Emoji' }

-- config.color_scheme = 'Ef-Light'
-- config.color_scheme = 'Raycast_Light'
config.color_scheme = 'Night Owlish Light'

config.enable_tab_bar = false
config.term = 'wezterm'
config.adjust_window_size_when_changing_font_size = false
-- config.default_cursor_style = 'SteadyBar'
config.mux_enable_ssh_agent = false

return config
