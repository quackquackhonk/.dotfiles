# Home manager manages some desktop entries
{
  pkgs,
  ...
}:
{
  # use emacs as a file manager
  home.file = {
    ".local/share/applications/emacsfm.desktop".source = ../desktop/emacsfm.desktop;
  };

  # XDG configuration
  xdg = {
    enable = true;
    systemDirs.data = ["/home/sahana/.local/share/applications/"];
    portal = {
      enable = true;
    };
    mimeApps = {
      enable = true;
      associations.added = {
        "application/x-directory" = ["emacsfm.desktop"];
        "inode/directory" = ["emacsfm.desktop"];
      };
      defaultApplications = {
        "application/x-directory" = ["emacsfm.desktop"];
        "inode/directory" = ["emacsfm.desktop"];
      };
    };

    desktopEntries = {
      poweroff = {
        name = "poweroff";
        exec = "poweroff";
      };
    };
  };

}
