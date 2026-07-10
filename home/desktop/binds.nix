{
  ...
}:
{
  # Hyprland bindings
  wayland.windowManager.hyprland = {
    settings = {
      "$shell" = "dms ipc call";

      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
      ];
      bind = [
        "$mod, Q, killactive,"
        "$mod, T, togglefloating"

        # Quick launch programs
        "$mod, B, exec, $browser"
        "$mod, D, exec, $discord"
        "$mod, F11, fullscreen, 0, toggle"
        "$mod, Return, exec, $terminal"
        "$mod, Escape, exec, $shell powermenu toggle"
        # Launder
        "$mod, SPACE, exec, $shell launcher toggle"
        # TODO: I want GUI SHIFT Space to launch cli commands
        # "$mod, SPACE, global, caelestia:launcher"

        # emacs
        "$mod, E, exec, $emacs"
        "$mod SHIFT, E, exec, emacsclient -e '(kill-emacs)'"

        # Move focus with arrow keys
        "$mod, left, movefocus, l"
        "$mod, right, movefocus, r"
        "$mod, up, movefocus, u"
        "$mod, down, movefocus, d"

        # Switch to a relative workspace
        "$mod, bracketright, workspace, r+1"
        "$mod, bracketleft, workspace, r-1"
        # Move focused window to a relative workspace
        "$mainMod+Shift, rightbracket, movetoworkspace, r+1"
        "$mainMod+Shift, leftbracket, movetoworkspace, r-1"

        # special workspace (scratchpad)
        "$mod, backslash, togglespecialworkspace, magic"
        "$mod SHIFT, backslash, movetoworkspace, special:magic"

        # Utilities
        "$mod+Shift, S, exec, dms screenshot" # Capture region (freeze)
        "$mod+Shift+Alt, S, exec, dms screenshot full" # Fullscreen capture > clipboard
        # "$mod+Alt, R, exec, caelestia record -s" # Record screen with sound
        # "Ctrl+Alt, R, exec, caelestia record" # Record screen
        # "$mod+Shift+Alt, R, exec, caelestia record -r" # Record region
      ]
      ++ (
        # workspaces
        # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
        builtins.concatLists (
          builtins.genList (
            i:
            let
              ws = i + 1;
            in
            [
              "$mod, code:1${toString i}, workspace, ${toString ws}"
              "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
            ]
          ) 9
        )
      );
      bindl = [
        # Media
        ", XF86AudioPlay, global, caelestia:mediaToggle"
        ", XF86AudioPause, global, caelestia:mediaToggle"
        ", XF86AudioNext, global, caelestia:mediaNext"
        ", XF86AudioPrev, global, caelestia:mediaPrev"
        ", XF86AudioStop, global, caelestia:mediaStop"

        # Sound
        ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"

        # Utilities
        ", Print, global, caelestia:screenshotFreeze" # capture reigon + freeze
      ];
      bindle = [
        ", XF86AudioRaiseVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86AudioLowerVolume, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
      ];
    };
  };

}
