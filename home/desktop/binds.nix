{
  lib,
  ...
}:
{
  # Hyprland bindings
  wayland.windowManager.hyprland = {
    settings =
      let
        lua = lib.generators.mkLuaInline;
        bind = key: action: {
          _args = [
            key
            (lua action)
          ];
        };
        bindo = key: action: opts: {
          _args = [
            key
            (lua action)
            (lua opts)
          ];
        };

        # bind shortcuts
        exec = cmd: ''hl.dsp.exec_cmd("${cmd}")'';
        ws = n: ''hl.dsp.focus({ workspace = "${n}" })'';
        mvws = n: ''hl.dsp.window.move({ workspace = "${n}" })'';
        special = ''hl.dsp.workspace.toggle_special()'';
        shell = cmd: ''hl.dsp.exec_cmd("dms ipc call ${cmd}")'';
        fs = ''hl.dsp.window.fullscreen({ mode = "fullscreen"})'';
        volumeUp = (exec "wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+");
        volumeMute = (exec "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle");
        volumeDown = (exec "wpctl set-mute @DEFAULT_AUDIO_SINK@ 0; wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-");


        # some global variables
        terminal = "ghostty";
        browser = "zen-twilight";
        discord = "ELECTRON_OZONE_PLATFORM_HINT= discord";
        emacs = "emacsclient -c -a=''";

        workspaceBinds = builtins.concatLists (
          builtins.genList (
            i:
            let
              w = i + 1;
            in
            [
              (bind "SUPER + ${toString w}" (ws (toString w)))
              (bind "SUPER + SHIFT + ${toString w}" (mvws (toString w)))
            ]
          ) 9
        );
      in
      {
        bind = [
          # common utils
          (bind "SUPER + Q" "hl.dsp.window.close()")
					(bind "SUPER + F11" fs)

					# Move windows with mouse drag
					(bindo "SUPER + mouse:272" ''hl.dsp.window.drag()'' ''{ drag = true }'')
          # TODO: resize window

          # Quick launch programs
					(bind "SUPER + B" (exec browser))
					(bind "SUPER + E" (exec emacs))
					(bind "SUPER + SHIFT + E" (exec "emacsclient -e '(kill-emacs)'"))
					(bind "SUPER + D" (exec discord))
					(bind "SUPER + Return" (exec terminal))

          # shell commands
					(bind "SUPER + Space" (shell "launcher toggle"))
					(bind "SUPER + Escape" (shell "powermenu toggle"))
          (bind "SUPER + SHIFT + S" (exec "dms screenshot"))
          (bind "SUPER + SHIFT + ALT + S" (exec "dms screenshot full"))

          # Move focus
          (bind "SUPER + left" ''hl.dsp.focus({ direction = "left" })'')
          (bind "SUPER + right" ''hl.dsp.focus({ direction = "right" })'')
          (bind "SUPER + up" ''hl.dsp.focus({ direction = "up" })'')
          (bind "SUPER + down" ''hl.dsp.focus({ direction = "down" })'')

          # Special workspace
          (bind "SUPER + backslash" special)
          (bind "SUPER + SHIFT + backslash" (mvws "special"))

          # Switch/Move to a relative workspace
          (bind "SUPER + bracketleft" (ws "e-1"))
          (bind "SUPER + bracketright" (ws "e+1"))
          (bind "SUPER + SHIFT + bracketleft" (ws "e-1"))
          (bind "SUPER + SHIFT + bracketright" (ws "e+1"))


          (bindo "XF86AudioLowerVolume" volumeUp ''{ locked = true, repeating = true }'')
					(bindo "XF86AudioMute" volumeMute ''{ locked = true }'')
					(bindo "XF86AudioMicMute" volumeDown ''{ locked = true, repeating = true}'')
        ] ++ workspaceBinds;
      };
  };

}
